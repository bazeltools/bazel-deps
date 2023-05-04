package com.github.johnynek.bazel_deps

import cats.Eval
import cats.arrow.FunctionK
import cats.free.Free
import cats.free.Free.liftF
import java.io.{File, FileOutputStream, FileInputStream, ByteArrayOutputStream, IOException}
import java.nio.file.{Files, SimpleFileVisitor, FileVisitResult, Path => JPath}
import java.nio.file.attribute.BasicFileAttributes
import java.util.Arrays
import java.util.regex.Pattern
import org.slf4j.LoggerFactory
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

import cats.implicits._

/** To enable mocking and testing, we keep IO abstract and then plug in
  * implementations
  */

object IO {
  private[this] val logger = LoggerFactory.getLogger("IO")

  val charset = "UTF-8"
  val pathSeparator = File.separator

  case class Path(parts: List[String]) {
    def child(p: String): Path = Path(parts ++ List(p))
    def parent: Path = Path(parts.dropRight(1))
    def sibling(p: String): Path = Path(parts.dropRight(1) ++ List(p))
    def asString: String = parts.mkString(pathSeparator)
    def extension: String = {
      val fileName = parts.last
      val segments = fileName.split("\\.")
      if(segments.length == 1) {
        fileName
      } else {
        segments.tail.mkString(".")
      }
    }
  }

  object Path {
    implicit val orderingPath: Ordering[Path] = Ordering.by { (p: Path) => p.parts }
  }

  def path(s: String): Path =
    Path(s.split(Pattern.quote(pathSeparator)).toList match {
      case "" :: rest => rest
      case list       => list
    })

  sealed abstract class Ops[+T]
  case class Exists(path: Path) extends Ops[Boolean]
  case class MkDirs(path: Path) extends Ops[Boolean]
  case class RmRf(path: Path, removeHidden: Boolean) extends Ops[Unit]
  /*
   * Since we generally only write data once, use Eval
   * here so by using `always` we can avoid holding
   * data longer than needed if that is desired.
   */
  case class WriteFile(f: Path, data: Eval[String]) extends Ops[Unit]
  case class WriteGzipFile(f: Path, data: Eval[String]) extends Ops[Unit]
  case class Failed(err: Throwable) extends Ops[Nothing]
  case class ReadFile(path: Path) extends Ops[Option[String]]

  type Result[T] = Free[Ops, T]

  val unit: Result[Unit] =
    const(())

  def const[T](t: T): Result[T] =
    Free.pure[Ops, T](t)

  def exists(f: Path): Result[Boolean] =
    liftF[Ops, Boolean](Exists(f))

  def failed[T](t: Throwable): Result[T] =
    liftF[Ops, T](Failed(t))

  def mkdirs(f: Path): Result[Boolean] =
    liftF[Ops, Boolean](MkDirs(f))

  def recursiveRm(path: Path, removeHidden: Boolean = true): Result[Unit] =
    liftF[Ops, Unit](RmRf(path, removeHidden))

  def recursiveRmF(path: Path, removeHidden: Boolean = true): Result[Unit] =
    exists(path).flatMap {
      case true  => recursiveRm(path, removeHidden)
      case false => unit
    }

  def writeUtf8(f: Path, s: => String): Result[Unit] =
    liftF[Ops, Unit](WriteFile(f, Eval.always(s)))

  def writeGzipUtf8(f: Path, s: => String): Result[Unit] =
    liftF[Ops, Unit](WriteGzipFile(f, Eval.always(s)))

  // Reads the contents of `f`, returning None if file doesn't exist
  def readUtf8(f: Path): Result[Option[String]] =
    liftF[Ops, Option[String]](ReadFile(f))

  def orUnit(optIO: Option[Result[Unit]]): Result[Unit] =
    optIO match {
      case Some(io) => io
      case None => IO.const(())
    }

  def fileSystemExec(root: File): FunctionK[Ops, Try] =
    new ReadWriteExec(root)

  final case class ReadOnlyException[A](op: Ops[A]) extends Exception(s"invalid op = $op in read-only mode")

  class ReadOnlyExec(root: File) extends FunctionK[Ops, Try] {
    require(root.isAbsolute, s"Absolute path required, found: $root")

    protected val successUnit: Try[Unit] = Success(())    

    def fileFor(p: Path): File =
      p.parts.foldLeft(root) { (p, element) => new File(p, element) }

    def apply[A](o: Ops[A]): Try[A] = o match {
      case Exists(f) => Try(fileFor(f).exists())
      case ReadFile(f) =>
        Try({
          val ff = fileFor(f)
          if (ff.exists)
            Some(new String(Files.readAllBytes(ff.toPath), charset))
          else None
        })
      case Failed(err) => Failure(err)
      // The following are not read only
      case _ => Failure(ReadOnlyException(o))
    }
  }

  class ReadWriteExec(root: File) extends ReadOnlyExec(root) {
    override def apply[A](o: Ops[A]): Try[A] = o match {
      case MkDirs(f) => Try(fileFor(f).mkdirs())
      case RmRf(f, removeHidden) =>
        Try {
          // get the java path
          val file = fileFor(f)
          val path = file.toPath
          if (!removeHidden) {
            Files.walkFileTree(
              path,
              new SimpleFileVisitor[JPath] {
                override def visitFile(
                    file: JPath,
                    attrs: BasicFileAttributes
                ) = {
                  if (file.getFileName.startsWith(".") && !removeHidden) { // Hidden!
                    throw new Exception(
                      s"Encountered hidden file ${file.getFileName}, and should not remove hidden files/folders. Aborting."
                    )
                  }
                  FileVisitResult.CONTINUE
                }
              }
            )
          }

          Files.walkFileTree(
            path,
            new SimpleFileVisitor[JPath] {
              override def visitFile(
                  file: JPath,
                  attrs: BasicFileAttributes
              ) = {
                Files.delete(file)
                FileVisitResult.CONTINUE
              }
              override def postVisitDirectory(
                  dir: JPath,
                  exc: IOException
              ) = {
                Files.delete(dir)
                FileVisitResult.CONTINUE
              }
            }
          )
          ()
        }
      case WriteFile(f, d) =>
        Try {
          val os = new FileOutputStream(fileFor(f))
          try os.write(d.value.getBytes(charset))
          finally { os.close() }
        }
      case WriteGzipFile(f, d) =>
        Try {
          import java.util.zip.GZIPOutputStream;
          val os = new GZIPOutputStream(new FileOutputStream(fileFor(f)))
          try os.write(d.value.getBytes(charset))
          finally { os.close() }
        }
      case _ => super.apply(o)
    }
  }

  sealed abstract class CheckException(val message: String) extends Exception(message) {
    def path: IO.Path
  }

  object CheckException {
    case class DirectoryMissing(path: IO.Path) extends CheckException(s"directory ${path.asString} expected but missing")
    case class WriteMismatch(path: IO.Path, current: Option[String], expected: String, compressed: Boolean) extends CheckException(
      (if (compressed) "compressed " else "") + s"file ${path.asString} does not " + (if (current.isEmpty) "exist." else "match.")
    )
  }
  // This reads, but doesn't write. Instead it errors if the
  // write has not already been done bit-for-bit identically
  class ReadCheckExec(root: File) extends ReadOnlyExec(root) {
    private[this] val lock = new AnyRef
    private[this] var ces: Vector[CheckException] = Vector.empty
    def checkExceptions(): Vector[CheckException] = lock.synchronized { ces }
    private def add(ce: CheckException): Unit =
      lock.synchronized { ces = ces :+ ce }

    def logErrorCount(onEx: CheckException => Unit): Int = {
      // this is reading mutable state, so it has to run
      // after foldMap
      val errs = checkExceptions()
      errs.sortBy(_.path).foreach(onEx)
      errs.size
    }
    override def apply[A](o: Ops[A]): Try[A] = o match {
      case MkDirs(f) =>
        try {
          if (fileFor(f).exists()) Success(false)
          else {
            add(CheckException.DirectoryMissing(f))
            Success(true)
          }
        }
        catch {
          case NonFatal(e) => Failure(e)
        }
      case RmRf(_, _) => successUnit
      case WriteFile(f, d) =>
        try {
          val expected = d.value
          val ff = fileFor(f)
          if (ff.exists) {
            val found = new String(Files.readAllBytes(ff.toPath), charset)
            if (found != expected) {
              add(CheckException.WriteMismatch(f, Some(found), expected, compressed = false))
            }
          }
          else {
            add(CheckException.WriteMismatch(f, None, expected, compressed = false)) 
          }
          successUnit
        }
        catch {
          case NonFatal(e) => Failure(e)
        }
      case WriteGzipFile(f, d) =>
        try {
          val ff = fileFor(f)
          val expected = d.value
          if (ff.exists) {
            import java.util.zip.GZIPInputStream;
            val is = new GZIPInputStream(new FileInputStream(ff))
            val os = new ByteArrayOutputStream
            val buffer = new Array[Byte](1 << 12) // 4k
            @annotation.tailrec
            def readAll(): Array[Byte] = {
              val count = is.read(buffer)
              if (count >= 0) {
                os.write(buffer, 0, count)
                readAll()
              }
              else {
                os.toByteArray()
              }
            }
            val found = new String(readAll(), charset)
            if (found != expected) {
              add(CheckException.WriteMismatch(f, Some(found), expected, compressed = true)) 
            }
          }
          else {
            add(CheckException.WriteMismatch(f, None, expected, compressed = true)) 
          }
          successUnit
        }
        catch {
          case NonFatal(e) => Failure(e)
        }
      case _ => super.apply(o)
    }
  }
}
