package com.github.johnynek.bazel_deps

import cats.data.Xor
import io.circe.jawn.JawnParser
import java.io.File
import scala.util.{ Failure, Success }

object ParseProject extends MakeDeps {
  def getSettings(args: Array[String]) = {
    val (workspacePath, projectRoot, fileName) = try {
      require(args.length == 3)
      (args(0), args(1), args(2))
    } catch {
      case t: Throwable =>
        List("Error, expected three args: path-to-workspace.bzl project-root-dir dependencies-file[.yaml|.json]",
          s"""found ${args.length} args: ${args.mkString(",")}""").foreach(System.err.println)
        System.exit(1)
        sys.error("unreachable")
    }
    val content = Model.readFile(new File(fileName)) match {
      case Success(str) => str
      case Failure(err) =>
        System.err.println(s"[ERROR]: Failed to read $fileName.\n$err")
        System.exit(1)
        sys.error("unreachable")
    }
    val parser = if (fileName.endsWith(".json")) new JawnParser else Yaml
    val model = Decoders.decodeModel(parser, content) match {
      case Xor.Right(m) => m
      case Xor.Left(err) =>
        System.err.println(s"[ERROR]: Failed to parse $fileName.\n$err")
        System.exit(1)
        sys.error("unreachable")
    }
    println(model)
    //(model, workspacePath, projectRoot)
    ???
  }
}
