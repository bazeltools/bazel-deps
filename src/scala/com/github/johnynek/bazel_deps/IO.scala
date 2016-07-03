package com.github.johnynek.bazel_deps

import cats.arrow.NaturalTransformation
import cats.data.Xor
import cats.Eval
import cats.free.Free
import cats.free.Free.liftF
import java.io.{File, FileOutputStream}
/**
 * To enable mocking and testing, we keep IO
 * abstract and then plug in implementations
 */

object IO {
  case class Path(parts: List[String]) {
    def child(p: String): Path = Path(parts ++ List(p))
  }

  sealed abstract class Ops[+T]
  case class Exists(path: Path) extends Ops[Boolean]
  case class MkDirs(path: Path) extends Ops[Boolean]
  /*
   * Since we generally only write data once, use Eval
   * here so by using `always` we can avoid holding
   * data longer than needed if that is desired.
   */
  case class WriteFile(f: Path, data: Eval[String]) extends Ops[Unit]

  type Result[T] = Free[Ops, T]

  def const[T](t: T): Result[T] =
    Free.pure[Ops, T](t)

  def exists(f: Path): Result[Boolean] =
    liftF[Ops, Boolean](Exists(f))

  def mkdirs(f: Path): Result[Boolean] =
    liftF[Ops, Boolean](MkDirs(f))

  def writeUtf8(f: Path, s: => String): Result[Unit] =
    liftF[Ops, Unit](WriteFile(f, Eval.always(s)))

  type XorTry[T] = Xor[Throwable, T]

  def fileSystemExec(root: File): NaturalTransformation[Ops, XorTry] = new NaturalTransformation[Ops, XorTry] {
    require(root.isAbsolute, s"Absolute path required, found: $root")

    def fileFor(p: Path): File =
      p.parts.foldLeft(root) { (p, element) => new File(p, element) }

    def apply[A](o: Ops[A]): Xor[Throwable, A] = o match {
      case Exists(f) => Xor.catchNonFatal(fileFor(f).exists())
      case MkDirs(f) => Xor.catchNonFatal(fileFor(f).mkdirs())
      case WriteFile(f, d) =>
        Xor.catchNonFatal {
          val os = new FileOutputStream(fileFor(f))
          try os.write(d.value.getBytes("UTF-8")) finally { os.close() }
        }
    }
  }
}
