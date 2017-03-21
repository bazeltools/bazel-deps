package com.github.johnynek.bazel_deps

import cats.arrow.NaturalTransformation
import cats.data.Xor
import cats.Eval
import cats.free.Free
import cats.free.Free.liftF
import java.io.{File, FileOutputStream, IOException}
import java.nio.file.{Files, SimpleFileVisitor, FileVisitResult, Path => JPath}
import java.nio.file.attribute.BasicFileAttributes
/**
 * To enable mocking and testing, we keep IO
 * abstract and then plug in implementations
 */

object IO {
  case class Path(parts: List[String]) {
    def child(p: String): Path = Path(parts ++ List(p))
  }

  def path(s: String): Path =
    Path(s.split("/").toList match {
      case "" :: rest => rest
      case list => list
    })

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

  type XorTry[T] = Xor[Throwable, T]

  def run[A](io: Result[A], root: File)(resume: A => Unit): Unit =
    io.foldMap(IO.fileSystemExec(root)) match {
      case Xor.Left(err) =>
        System.err.println(err)
        System.exit(-1)
      case Xor.Right(result) =>
        resume(result)
        System.exit(0)
    }

  def fileSystemExec(root: File): NaturalTransformation[Ops, XorTry] = new NaturalTransformation[Ops, XorTry] {
    require(root.isAbsolute, s"Absolute path required, found: $root")

    def fileFor(p: Path): File =
      p.parts.foldLeft(root) { (p, element) => new File(p, element) }

    def apply[A](o: Ops[A]): Xor[Throwable, A] = o match {
      case Exists(f) => Xor.catchNonFatal(fileFor(f).exists())
      case MkDirs(f) => Xor.catchNonFatal(fileFor(f).mkdirs())
      case RmRf(f) => Xor.catchNonFatal {
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
        Xor.catchNonFatal {
          val os = new FileOutputStream(fileFor(f))
          try os.write(d.value.getBytes("UTF-8")) finally { os.close() }
        }
      case Failed(err) => Xor.left(err)
    }
  }
}
