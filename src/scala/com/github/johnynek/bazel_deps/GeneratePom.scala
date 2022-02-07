package com.github.johnynek.bazel_deps

import java.io.File

import scala.util.{Failure, Success}

object GeneratePom {
  def readModel(path: File): Either[String, Model] = {
    val content: Either[String, String] =
      Model.readFile(path) match {
        case Success(str) => Right(str)
        case Failure(err) =>
          Left(s"[ERROR]: Failed to read ${path}.\n$err")
      }

    content.right.flatMap { c =>
      Decoders.decodeModel(Yaml, c) match {
        case Right(m) => Right(m)
        case Left(err) => Left(s"[ERROR]: Failed to parse ${path}.\n$err")
      }
    }
  }

  def apply(deps: File, pomFile: File): Unit = {
    val model = readModel(deps) match {
      case Left(msg) =>
        System.err.println(msg)
        System.exit(1)
        sys.error("unreachable")
      case Right(m) => m
    }

    val xml = model.toXml
    val p = new scala.xml.PrettyPrinter(80, 2)
    scala.xml.XML.save(
      pomFile.toString,
      scala.xml.XML.loadString(p.format(xml))
    )
  }
}
