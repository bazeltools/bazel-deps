package com.github.johnynek.bazel_deps

import cats.data.Xor
import io.circe.jawn.JawnParser
import java.io.File
import scala.util.{ Failure, Success }

object ParseProject extends MakeDeps {
  def getSettings(args: Array[String]) = {
    val (projectRoot, workspacePath, deps) = try {
      require(args.length == 3)
      val wp = args(1)
      val wpList = wp.split('/').toList
      require(wpList(0).nonEmpty)
      val root = new File(args(0))
      require(root.isAbsolute, s"Absolute path required, found: $root")
      require(root.isDirectory, s"$root is not a directory")
      require(root.exists, s"$root does not exist")
      (root, wpList, args(2))
    } catch {
      case t: Throwable =>
        List("Error, expected three args: project-root-dir relative-path-to-workspace.bzl relative-dependencies-file[.yaml|.json]",
          s"""found ${args.length} args: ${args.mkString(",")}""").foreach(System.err.println)
        System.exit(1)
        sys.error("unreachable")
    }
    val content = Model.readFile(new File(projectRoot, deps)) match {
      case Success(str) => str
      case Failure(err) =>
        System.err.println(s"[ERROR]: Failed to read $deps.\n$err")
        System.exit(1)
        sys.error("unreachable")
    }
    val parser = if (deps.endsWith(".json")) new JawnParser else Yaml
    val model = Decoders.decodeModel(parser, content) match {
      case Xor.Right(m) => m
      case Xor.Left(err) =>
        System.err.println(s"[ERROR]: Failed to parse $deps.\n$err")
        System.exit(1)
        sys.error("unreachable")
    }
    (model, workspacePath, projectRoot)
  }
}
