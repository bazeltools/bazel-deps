package com.github.johnynek.bazel_deps

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.Foldable
import cats.implicits._
import io.circe.jawn.JawnParser
import java.io.PrintWriter
import scala.util.{Failure, Success}
import java.nio.file.Path

object MergeDeps {
  private def load(f: Path): ValidatedNel[String, Model] =
    FormatDeps.readModel(f) match {
      case Right(m)  => Validated.valid(m)
      case Left(err) => Validated.invalidNel(err)
    }

  def fail(errs: NonEmptyList[String]): Nothing = {
    errs.toList.foreach(System.err.println)
    System.exit(1)
    sys.error("unreachable")
  }

  def apply(models: NonEmptyList[Path], out: Option[Path]): Unit = {

    type A[T] = ValidatedNel[String, T]
    val mod = models.traverse[A, Model](load).toEither.right.flatMap {
      Model.combine(_)
    }

    mod match {
      case Left(errs) => fail(errs)
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

  def addDep(
      model: Path,
      lang: Language,
      coords: NonEmptyList[MavenCoordinate]
  ): Unit =
    load(model) match {
      case Validated.Invalid(errs) => fail(errs)
      case Validated.Valid(m) =>
        val realLang = m.getOptions.replaceLang(lang)
        val deps = coords.map(realLang.unmangle(_).toDependencies(realLang))

        def combine(
            d1: Dependencies,
            d2: Dependencies
        ): Either[NonEmptyList[String], Dependencies] =
          Dependencies
            .combine(m.getOptions.getVersionConflictPolicy, d1, d2)
            .toEither

        type E[T] = Either[NonEmptyList[String], T]
        Foldable[NonEmptyList].foldM[E, Dependencies, Dependencies](
          deps,
          m.dependencies
        )(combine) match {
          case Left(errs) => fail(errs)
          case Right(resDep) =>
            val stream = m.copy(dependencies = resDep).toDoc.renderStream(100)
            val pw = new PrintWriter(model.toFile)
            stream.foreach(pw.print(_))
            pw.flush
            pw.close
        }
    }
}
