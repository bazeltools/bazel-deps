package com.github.johnynek.bazel_deps

import cats.data.NonEmptyList
import cats.implicits._
import com.monovore.decline.{ Command => DCommand, _ }
import java.io.File
import java.nio.file.Path

sealed abstract class Command

object Command {
  case class Generate(repoRoot: Path, depsFile: String, shaFile: String) extends Command {
    def absDepsFile: File =
      new File(repoRoot.toFile, depsFile)

    def shaFilePath: String =
      new File(repoRoot.toFile, shaFile).toString
  }
  val generate = DCommand("generate", "generate transitive bazel targets") {
    val repoRoot = Opts.option[Path](
      "repo-root",
      short = "r",
      metavar = "reporoot",
      help = "the ABSOLUTE path to the root of the bazel repo")

    val depsFile = Opts.option[String](
      "deps",
      short = "d",
      metavar = "deps",
      help = "relative path to the dependencies yaml file")

    val shaFile = Opts.option[String](
      "sha-file",
      short = "s",
      metavar = "sha-file",
      help = "relative path to the sha lock file (usually called workspace.bzl).")

    (repoRoot |@| depsFile |@| shaFile).map(Generate(_, _, _))
  }

  case class FormatDeps(deps: Path, overwrite: Boolean) extends Command
  val format = DCommand("format-deps", "format the dependencies yaml file") {
    val depsFile = Opts.option[Path]("deps", short = "d", help = "the ABSOLUTE path to your dependencies yaml file")
    val overwrite = Opts.flag("overwrite", short = "o", help = "if set, we overwrite the file after we read it").orFalse

    (depsFile |@| overwrite).map(FormatDeps(_, _))
  }

  case class MergeDeps(deps: NonEmptyList[Path], output: Option[Path]) extends Command
  val mergeDeps = DCommand("merge-deps", "merge a series of dependencies yaml file") {
    val deps = Opts.options[Path]("deps", short = "d", help = "list of ABSOLUTE paths of files to merge")
    val out = Opts.option[Path]("output", short = "o", help = "merged output file").orNone

    (deps |@| out).map(MergeDeps(_, _))
  }


  val command: DCommand[Command] =
    DCommand(name = "bazel-deps", header = "a tool to manage transitive external Maven dependencies for bazel") {
      (Opts.help :: (List(generate, format, mergeDeps).map(Opts.subcommand(_))))
        .reduce(_.orElse(_))
    }
}
