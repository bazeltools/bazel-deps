package com.github.johnynek.bazel_deps

import cats.data.{ NonEmptyList, Validated, ValidatedNel }
import cats.implicits._
import io.circe.jawn.JawnParser
import java.io.{ File, PrintWriter }
import scala.util.{ Failure, Success }

object MergeDeps {
  def apply(models: List[File], out: Option[File]): Unit = {
    def load(f: File): ValidatedNel[String, Model] =
      FormatDeps.readModel(f) match {
        case Right(m) => Validated.valid(m)
        case Left(err) => Validated.invalidNel(err)
      }

    type A[T] = ValidatedNel[String, T]
    val mod = models.traverse[A, Model](load).toEither.right.flatMap {
      case Nil => Left(NonEmptyList.of("empty model list, require at least one"))
      case h :: tail =>
        Model.combine(NonEmptyList(h, tail))
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
            val pw = new PrintWriter(path)
            stream.foreach(pw.print(_))
            pw.flush
            pw.close
        }
      }
  }
}
