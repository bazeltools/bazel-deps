package com.github.johnynek.bazel_deps

import cats.effect.{ ExitCode, IO, IOApp }
import org.slf4j.impl.SimpleLogger.DEFAULT_LOG_LEVEL_KEY

object ParseProject extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    IO(Command.command.parse(args)).flatMap {
      case Left(help) =>
        IO.blocking {
          System.err.println(help)
          if (help.errors.isEmpty) ExitCode.Success
          else ExitCode(1)
        }
      case Right(command) =>
        IO {
          val level = command.verbosity.repr.toUpperCase
          System.setProperty(DEFAULT_LOG_LEVEL_KEY, level)
        }.flatMap { _ =>
          command match {
            case gen: Command.Generate =>
              MakeDeps(gen)
            case gen: Command.FormatDeps =>
              FormatDeps(gen.deps, gen.overwrite)
            case Command.MergeDeps(ms, out, _) =>
              MergeDeps(ms, out)
            case Command.AddDep(yaml, lang, coords, _) =>
              MergeDeps.addDep(yaml, lang, coords)
          }
        }
    }
  }
}
