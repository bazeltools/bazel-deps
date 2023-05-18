package com.github.johnynek.bazel_deps

import cats.data.{EitherT, NonEmptyList, Validated, ValidatedNel}
import cats.effect.{ ExitCode, IO }
import cats.{Applicative, Foldable, Parallel}
import cats.implicits._
import io.circe.jawn.JawnParser
import java.io.PrintWriter
import scala.util.{Failure, Success}
import java.nio.file.Path

object MergeDeps {
  private def load(f: Path): IO[ValidatedNel[String, Model]] =
    FormatDeps.readModel(f).map {
      case Right(m)  => Validated.valid(m)
      case Left(err) => Validated.invalidNel(err)
    }

  private def loadAll(models: NonEmptyList[Path]): IO[ValidatedNel[String, NonEmptyList[Model]]] = {
    type V[T] = ValidatedNel[String, T]
    type A[T] = IO.Par[V[T]]
    implicit val appA: Applicative[A] = Parallel[IO].applicative.compose(Applicative[V])

    Parallel[IO].sequential(models.traverse[A, Model] { p => Parallel[IO].parallel(load(p)) })
  }

  private def fail(errs: NonEmptyList[String]): IO[ExitCode]  = IO.blocking {
    errs.toList.foreach(System.err.println)
    ExitCode(1)
  }

  def apply(models: NonEmptyList[Path], out: Option[Path]): IO[ExitCode] =
    loadAll(models)
      .flatMap { validated =>
        validated.toEither.flatMap(Model.combine) match {
          case Left(errs) => fail(errs)
          case Right(m) =>
            val stream = m.toDoc.renderStreamTrim(100)
            IO.blocking {
              out match {
                case None => stream.foreach(System.out.print)
                case Some(path) =>
                  val pw = new PrintWriter(path.toFile)
                  stream.foreach(pw.print(_))
                  pw.flush
                  pw.close
              }
              ExitCode.Success
            }
        }
      }

  def addDep(
      model: Path,
      lang: Language,
      coords: NonEmptyList[MavenCoordinate]
  ): IO[ExitCode] =
    load(model).flatMap {
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
          case Right(resDep) =>
            val stream = m.copy(dependencies = resDep).toDoc.renderStreamTrim(100)
            IO.blocking {
              val pw = new PrintWriter(model.toFile)
              try {
                stream.foreach(pw.print(_))
                pw.flush
              }
              finally {
                pw.close
              }
              ExitCode.Success
            }
          case Left(errs) => fail(errs)
        }
    }
}
