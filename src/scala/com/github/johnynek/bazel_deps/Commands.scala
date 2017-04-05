package com.github.johnynek.bazel_deps

import org.rogach.scallop._
import org.rogach.scallop.exceptions.ScallopException
import java.io.File

sealed abstract class Command {
  def validate: Either[String, Command]
}

object Command {
  case class Generate(repoRoot: File, depsFile: String, shaFilePath: List[String]) extends Command {

    def absDepsFile: File = new File(repoRoot, depsFile)

    def validate = {
      def test(b: Boolean, msg: => String): Option[String] = if (!b) Some(msg) else None

      val slash = "/"
      test(repoRoot.isAbsolute, s"Absolute path required, found: $repoRoot")
        .orElse(test(repoRoot.isDirectory, s"$repoRoot is not a directory"))
        .orElse(test(repoRoot.exists, s"$repoRoot does not exist"))
        .orElse(test(shaFilePath.headOption.exists(_.nonEmpty),
          s"Expected relative path for sha-file, found ${shaFilePath.mkString(slash)}")) match {
            case None => Right(this)
            case Some(err) => Left(err)
          }
    }
  }

  case class FormatDeps(depsFile: File, overwrite: Boolean) extends Command {
    def validate =
      if (!depsFile.exists) Left(s"$depsFile does not exist")
      else if (!depsFile.isFile) Left(s"$depsFile is not a file")
      else Right(this)
  }
}

class CommandParser(args: Seq[String]) extends ScallopConf(args) {
  banner("bazel-deps: java/scala dependency lockfile generator for bazel")
  footer("see github: https://github.com/johnynek/bazel-deps")

  object Gen3rdParty extends Subcommand("generate") {
    val repoRoot = opt[File](name = "repo-root", required = true, descr = "the ABSOLUTE path to the root of the bazel repo")
    val depsFile = opt[String](name = "deps", required = true, descr = "relative path to the dependencies yaml file")
    val shaFile = opt[String](name = "sha-file", required = true, descr = "relative path to the sha lock file (usually called workspace.bzl).")
  }
  addSubcommand(Gen3rdParty)

  object FmtDeps extends Subcommand("format-deps") {
    val depsFile = opt[File](name = "deps", required = true, descr = "the ABSOLUTE path to your dependencies yaml file")
    val overwrite = opt[Boolean](name = "overwrite", descr = "if set, we overwrite the file after we read it")
  }
  addSubcommand(FmtDeps)

  verify()
  override def onError(t: Throwable): Unit = t match {
    case ScallopException(message) =>
      errorMessageHandler(s"$message\ntry running --help")
    case other => super.onError(other)
  }

  def getCommand: Either[String, Command] =
    subcommand match {
      case Some(gen@Gen3rdParty) =>
        val str = gen.shaFile()
        val wpList = str.split('/').toList
        val c = Command.Generate(gen.repoRoot(), gen.depsFile(), wpList)
        c.validate
      case Some(gen@FmtDeps) =>
        val c = Command.FormatDeps(gen.depsFile(), gen.overwrite())
        c.validate
      case None =>
        Left("expected subcommand, found None")
      case other =>
        Left("unexpected subcommand, found $other")
    }
}
