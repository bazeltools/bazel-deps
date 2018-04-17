package com.github.johnynek.bazel_deps

object ParseProject {
  def main(args: Array[String]): Unit = {
    Command.command.parse(args) match {
      case Left(help) =>
        System.err.println(help)
        if (help.errors.isEmpty) System.exit(0)
        else System.exit(1)
      case Right(gen: Command.Generate) =>
        val level = gen.verbosity.repr.toUpperCase
        System.setProperty(org.slf4j.impl.SimpleLogger.DEFAULT_LOG_LEVEL_KEY, level)
        MakeDeps(gen)
      case Right(gen: Command.FormatDeps) =>
        FormatDeps(gen.deps.toFile, gen.overwrite)
      case Right(Command.MergeDeps(ms, out)) =>
        MergeDeps(ms, out)
      case Right(Command.AddDep(yaml, lang, coords)) =>
        MergeDeps.addDep(yaml, lang, coords)
    }
  }
}
