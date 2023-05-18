package com.github.johnynek.bazel_deps

import cats.effect.{ ExitCode, IO }
import java.io.PrintWriter
import java.nio.file.{ Files, Path }
import io.circe.jawn.JawnParser
import scala.util.{Failure, Success}

object FormatDeps {
  private def read(p: Path): IO[String] =
    IO.blocking(new String(Files.readAllBytes(p), "UTF-8"))

  def readModel(path: Path): IO[Either[String, Model]] =
    read(path).attempt
    .map {
      case Right(content) =>
        val parser = if (path.toString.endsWith(".json")) new JawnParser else Yaml
        Decoders.decodeModel(parser, content) match {
          case Right(m)  => Right(m)
          case Left(err) => Left(s"[ERROR]: Failed to parse ${path}.\n$err")
        }
      case Left(err) =>
        Left(s"[ERROR]: Failed to read ${path}.\n$err")
    }

  def apply(path: Path, overwrite: Boolean): IO[ExitCode] =
    readModel(path).flatMap {
      case Left(msg) =>
        IO.blocking {
          System.err.println(msg)
          ExitCode(1)
        }
      case Right(model) =>
        val stream = model.toDoc.renderStreamTrim(100)
        IO.blocking {
          if (overwrite) {
            val pw = new PrintWriter(path.toFile)
            stream.foreach(pw.print(_))
            pw.flush
            pw.close
          } else {
            stream.foreach(System.out.print(_))
          }
          ExitCode.Success
        }
      }
}
