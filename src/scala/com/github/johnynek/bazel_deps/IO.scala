package com.github.johnynek.bazel_deps

import cats.arrow.NaturalTransformation
import cats.data.Xor
import cats.free.Free
import cats.free.Free.liftF
import java.io.{File, FileOutputStream}
/**
 * To enable mocking and testing, we keep IO
 * abstract and then plug in implementations
 */

object IO {
  sealed abstract class Ops[+T]
  case class Exists(path: File) extends Ops[Boolean]
  case class MkDirs(path: File) extends Ops[Boolean]
  case class WriteFile(f: File, data: Array[Byte]) extends Ops[Unit]

  type Result[T] = Free[Ops, T]

  def const[T](t: T): Result[T] =
    Free.pure[Ops, T](t)

  def exists(f: File): Result[Boolean] =
    liftF[Ops, Boolean](Exists(f))

  def mkdirs(f: File): Result[Boolean] =
    liftF[Ops, Boolean](MkDirs(f))

  def write(f: File, d: Array[Byte]): Result[Unit] =
    liftF[Ops, Unit](WriteFile(f, d))

  type XorTry[T] = Xor[Throwable, T]

  def fileSystemExec: NaturalTransformation[Ops, XorTry] = new NaturalTransformation[Ops, XorTry] {
    def apply[A](o: Ops[A]): Xor[Throwable, A] = o match {
      case Exists(f) => Xor.catchNonFatal(f.exists())
      case MkDirs(f) => Xor.catchNonFatal(f.mkdirs())
      case WriteFile(f, d) =>
        Xor.catchNonFatal {
          val os = new FileOutputStream(f)
          try os.write(d) finally { os.close() }
        }
    }
  }
}
