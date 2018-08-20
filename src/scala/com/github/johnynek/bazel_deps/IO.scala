package com.github.johnynek.bazel_deps

import cats.Eval
import cats.arrow.FunctionK
import cats.free.Free
import cats.free.Free.liftF
import java.io.{File, FileOutputStream, IOException}
import java.nio.file.{Files, SimpleFileVisitor, FileVisitResult, Path => JPath}
import java.nio.file.attribute.BasicFileAttributes
import java.util.Arrays
import java.util.regex.Pattern
import org.slf4j.LoggerFactory
import scala.util.{ Failure, Success, Try }

import cats.implicits._
/**
 * To enable mocking and testing, we keep IO
 * abstract and then plug in implementations
 */

object IO {
  val charset = "UTF-8"
  val pathSeparator = File.separator

  private[this] val logger = LoggerFactory.getLogger("IO")

  case class Path(parts: List[String]) {
    def child(p: String): Path = Path(parts ++ List(p))
    def parent: Path = Path(parts.dropRight(1))
    def sibling(p: String): Path = Path(parts.dropRight(1) ++ List(p))
    def asString: String = parts.mkString(pathSeparator)
  }

  def path(s: String): Path =
    Path(s.split(Pattern.quote(pathSeparator)).toList match {
      case "" :: rest => rest
      case list => list
    })

  case class FileComparison(path: Path, ok: Boolean)

  sealed abstract class Ops[+T]
  case class Exists(path: Path) extends Ops[Boolean]
  case class MkDirs(path: Path) extends Ops[Boolean]
  case class RmRf(path: Path) extends Ops[Unit]
  /*
   * Since we generally only write data once, use Eval
   * here so by using `always` we can avoid holding
   * data longer than needed if that is desired.
   */
  case class WriteFile(f: Path, data: Eval[String]) extends Ops[Unit]
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

  def recursiveRm(path: Path): Result[Unit] =
    liftF[Ops, Unit](RmRf(path))

  def recursiveRmF(path: Path): Result[Unit] =
    exists(path).flatMap {
      case true => recursiveRm(path)
      case false => unit
    }

  def writeUtf8(f: Path, s: => String): Result[Unit] =
    liftF[Ops, Unit](WriteFile(f, Eval.always(s)))

  // Reads the contents of `f`, returning None if file doesn't exist
  def readUtf8(f: Path): Result[Option[String]] =
    liftF[Ops, Option[String]](ReadFile(f))

  // Checks if the path at `f` exists and has the content `s`
  def compare(f: Path, s: => String): Result[FileComparison] =
    readUtf8(f).map { contents => FileComparison(f, contents.map(s == _).getOrElse(false)) }

  def run[A](io: Result[A], root: File)(resume: A => Unit): Unit =
    io.foldMap(IO.fileSystemExec(root)) match {
      case Failure(err) =>
        logger.error("Failure during IO:", err)
        System.exit(-1)
      case Success(result) =>
        resume(result)
        System.exit(0)
    }

  def fileSystemExec(root: File): FunctionK[Ops, Try] = new FunctionK[Ops, Try] {
    require(root.isAbsolute, s"Absolute path required, found: $root")

    def fileFor(p: Path): File =
      p.parts.foldLeft(root) { (p, element) => new File(p, element) }

    def apply[A](o: Ops[A]): Try[A] = o match {
      case Exists(f) => Try(fileFor(f).exists())
      case MkDirs(f) => Try(fileFor(f).mkdirs())
      case RmRf(f) => Try {
        // get the java path
        val file = fileFor(f)
        //require(file.isDirectory, s"$f is not a directory")
        val path = file.toPath
        Files.walkFileTree(path, new SimpleFileVisitor[JPath] {
          override def visitFile(file: JPath, attrs: BasicFileAttributes) = {
            Files.delete(file)
            FileVisitResult.CONTINUE
          }
          override def postVisitDirectory(dir: JPath, exc: IOException) = {
            Files.delete(dir)
            FileVisitResult.CONTINUE
          }})
        ()
      }
      case WriteFile(f, d) =>
        Try {
          val os = new FileOutputStream(fileFor(f))
          try os.write(d.value.getBytes(charset)) finally { os.close() }
        }
      case ReadFile(f) => Try({
        val ff = fileFor(f)
        if (ff.exists) Some(new String(Files.readAllBytes(ff.toPath), charset)) else None
      })
      case Failed(err) => Failure(err)
    }
  }
}
