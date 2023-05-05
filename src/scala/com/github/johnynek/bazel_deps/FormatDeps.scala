package com.github.johnynek.bazel_deps

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

  def apply(path: Path, overwrite: Boolean): Unit = {
    val model = readModel(path) match {
      case Left(msg) =>
        System.err.println(msg)
        System.exit(1)
        sys.error("unreachable")
      case Right(m) => m
    }

    val stream = model.toDoc.renderStreamTrim(100)
    if (overwrite) {
      val pw = new PrintWriter(path.toFile)
      stream.foreach(pw.print(_))
      pw.flush
      pw.close
    } else {
      stream.foreach(System.out.print(_))
    }
  }
}
