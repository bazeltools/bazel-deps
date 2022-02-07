package com.github.johnynek.bazel_deps

import org.slf4j.impl.SimpleLogger.DEFAULT_LOG_LEVEL_KEY

object ParseProject {
  def main(args: Array[String]): Unit = {
    Command.command.parse(args) match {
      case Left(help) =>
        System.err.println(help)
        if (help.errors.isEmpty) System.exit(0)
        else System.exit(1)
      case Right(command) =>
        val level = command.verbosity.repr.toUpperCase
        System.setProperty(DEFAULT_LOG_LEVEL_KEY, level)
        command match {
          case gen: Command.Generate =>
            MakeDeps(gen)
          case gen: Command.FormatDeps =>
            FormatDeps(gen.deps.toFile, gen.overwrite)
          case Command.MergeDeps(ms, out, _) =>
            MergeDeps(ms, out)
          case Command.AddDep(yaml, lang, coords, _) =>
            MergeDeps.addDep(yaml, lang, coords)
          case gen: Command.GeneratePom =>
            GeneratePom(gen.deps.toFile, gen.pomFile.toFile, gen.groupId, gen.artifactId, gen.version)
        }
    }
  }
}
