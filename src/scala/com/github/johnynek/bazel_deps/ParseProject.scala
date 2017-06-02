package com.github.johnynek.bazel_deps

object ParseProject {
  def main(args: Array[String]): Unit = {
    Command.command.parse(args) match {
      case Left(help) =>
        System.err.println(help)
        if (help.errors.isEmpty) System.exit(0)
        else System.exit(1)
      case Right(gen: Command.Generate) =>
        MakeDeps(gen)
      case Right(gen: Command.FormatDeps) =>
        FormatDeps(gen.deps.toFile, gen.overwrite)
      case Right(Command.MergeDeps(ms, out)) =>
        MergeDeps(ms, out)
    }
  }
}
