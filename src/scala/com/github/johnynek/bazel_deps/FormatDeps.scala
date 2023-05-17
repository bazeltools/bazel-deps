package com.github.johnynek.bazel_deps

import cats.effect.{ ExitCode, IO }
import java.io.PrintWriter
import java.nio.file.Path
import io.circe.jawn.JawnParser
import scala.util.{Failure, Success}

object FormatDeps {
  def readModel(path: Path): Either[String, Model] = {
    val content: Either[String, String] =
      Model.readFile(path) match {
        case Success(str) => Right(str)
        case Failure(err) =>
          Left(s"[ERROR]: Failed to read ${path}.\n$err")
      }

    val parser = if (path.toString.endsWith(".json")) new JawnParser else Yaml
    content.right.flatMap { c =>
      Decoders.decodeModel(parser, c) match {
        case Right(m)  => Right(m)
        case Left(err) => Left(s"[ERROR]: Failed to parse ${path}.\n$err")
      }
    }
  }

  def apply(path: Path, overwrite: Boolean): IO[ExitCode] = IO.blocking {
    readModel(path) match {
      case Left(msg) =>
        System.err.println(msg)
        ExitCode(1)
      case Right(model) =>
        val stream = model.toDoc.renderStreamTrim(100)
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
