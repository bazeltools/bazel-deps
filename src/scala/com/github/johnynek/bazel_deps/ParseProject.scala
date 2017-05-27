package com.github.johnynek.bazel_deps

object ParseProject {
  def main(args: Array[String]): Unit = {
    val cmd = new CommandParser(args)

    cmd.getCommand match {
      case Left(err) =>
        System.err.println(err)
        cmd.printHelp()
        System.exit(1)
      case Right(gen: Command.Generate) =>
        MakeDeps(gen)
      case Right(gen: Command.FormatDeps) =>
        FormatDeps(gen.depsFile, gen.overwrite)
      case Right(Command.MergeDeps(ms, out)) =>
        MergeDeps(ms, out)
    }
  }
}
