package com.github.johnynek.bazel_deps

import java.io.{ File, PrintWriter }
import cats.data.Xor
import io.circe.jawn.JawnParser
import scala.util.{ Failure, Success }

object FormatDeps {
  def apply(path: File, overwrite: Boolean): Unit = {
    val content: String = Model.readFile(path) match {
      case Success(str) => str
      case Failure(err) =>
        System.err.println(s"[ERROR]: Failed to read ${path}.\n$err")
        System.exit(1)
        sys.error("unreachable")
    }

    val parser = if (path.toString.endsWith(".json")) new JawnParser else Yaml
    val model = Decoders.decodeModel(parser, content) match {
      case Xor.Right(m) => m
      case Xor.Left(err) =>
        System.err.println(s"[ERROR]: Failed to parse ${path}.\n$err")
        System.exit(1)
        sys.error("unreachable")
    }

    val stream = model.toDoc.renderStream(100)
    if (overwrite) {
      val pw = new PrintWriter(path)
      stream.foreach(pw.print(_))
      pw.flush
      pw.close
    }
    else {
      stream.foreach(System.out.print(_))
    }
  }
}
