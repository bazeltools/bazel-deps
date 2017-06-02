package com.github.johnynek.bazel_deps

import cats.data.{ NonEmptyList, Validated, ValidatedNel }
import cats.implicits._
import io.circe.jawn.JawnParser
import java.io.{ File, PrintWriter }
import scala.util.{ Failure, Success }
import java.nio.file.Path

object MergeDeps {
  def apply(models: NonEmptyList[Path], out: Option[Path]): Unit = {
    def load(f: Path): ValidatedNel[String, Model] =
      FormatDeps.readModel(f.toFile) match {
        case Right(m) => Validated.valid(m)
        case Left(err) => Validated.invalidNel(err)
      }

    type A[T] = ValidatedNel[String, T]
    val mod = models.traverse[A, Model](load).toEither.right.flatMap {
      Model.combine(_)
    }

    mod match {
      case Left(errs) =>
        errs.toList.foreach(System.err.println)
        System.exit(1)
        sys.error("unreachable")
      case Right(m) =>
        val stream = m.toDoc.renderStream(100)
        out match {
          case None => stream.foreach(System.out.print)
          case Some(path) =>
            val pw = new PrintWriter(path.toFile)
            stream.foreach(pw.print(_))
            pw.flush
            pw.close
        }
      }
  }
}
