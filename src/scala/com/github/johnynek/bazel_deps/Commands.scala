package com.github.johnynek.bazel_deps

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import com.monovore.decline.{Argument, Command => DCommand, _}
import java.io.File
import java.nio.file.Path

sealed abstract class Verbosity(val repr: String, val level: Int)

object Verbosity {
  case object Error extends Verbosity("error", 1)
  case object Warn extends Verbosity("warn", 2)
  case object Info extends Verbosity("info", 3)
  case object Debug extends Verbosity("debug", 4)
  case object Trace extends Verbosity("trace", 5)

  val levels: Map[String, Verbosity] =
    Map(
      "error" -> Error,
      "warn" -> Warn,
      "info" -> Info,
      "debug" -> Debug,
      "trace" -> Trace
    )

  private[this] val names: String =
    levels.values.toList.sortBy(_.level).map(_.repr).mkString(", ")

  val helpMessage: String =
    s"How verbose to log at (one of: $names, default: warn)."

  def errorMessage(s: String): String =
    s"Invalid verbosity level '$s'." + "\n" + s"Valid verbosity levels are: $names."

  implicit object VerbosityArgument extends Argument[Verbosity] {
    def defaultMetavar: String = "LEVEL"
    def read(s: String): ValidatedNel[String, Verbosity] =
      levels.get(s.toLowerCase) match {
        case Some(v) => Validated.valid(v)
        case None    => Validated.invalidNel(errorMessage(s))
      }
  }

  val opt = Opts
    .option[Verbosity](
      "verbosity",
      short = "v",
      help = Verbosity.helpMessage
    )
    .orElse(Opts(Verbosity.Warn))
}

sealed abstract class Command {
  def verbosity: Verbosity
}

object Command {
  case class Generate(
      repoRoot: Path,
      depsFile: String,
      resolvedOutput: Option[String],
      shaFile: String,
      targetFile: Option[String],
      buildifier: Option[String],
      pomFile: Option[String],
      checkOnly: Boolean,
      verbosity: Verbosity,
      disable3rdPartyInRepo: Boolean
  ) extends Command {

    def enable3rdPartyInRepo: Boolean = !disable3rdPartyInRepo
    def absDepsFile: File =
      new File(repoRoot.toFile, depsFile)

    def shaFilePath: String =
      new File(shaFile).toString
  }
  val generate = DCommand("generate", "generate transitive bazel targets") {
    val repoRoot = Opts.option[Path](
      "repo-root",
      short = "r",
      metavar = "reporoot",
      help = "the ABSOLUTE path to the root of the bazel repo"
    )

    val depsFile = Opts.option[String](
      "deps",
      short = "d",
      metavar = "deps",
      help = "relative path to the dependencies yaml file"
    )

    val resolvedOutput = Opts.option[String](
      "resolved-output",
      metavar = "resolved-output",
      help =
        "relative path to the file to emit target info into (usually called resolvedOutput.json)."
    ).orNone

    val shaFile = Opts.option[String](
      "sha-file",
      short = "s",
      metavar = "sha-file",
      help = "relative path to the sha lock file (usually called workspace.bzl).")


    val targetFile = Opts.option[String](
      "target-file",
      short = "t",
      metavar = "target-file",
      help = "relative path to the file to emit target info into (usually called target_file.bzl).").orNone


    val buildifier = Opts.option[String](
      "buildifier",
      metavar = "buildifier",
      help = "absolute path to buildifier binary, which will be called to format each generated BUILD file").orNone

    val pomFile = Opts.option[String](
      "pom-file",
      short = "p",
      metavar = "pom-file",
      help = "absolute path to the pom xml file").orNone

    val checkOnly = Opts.flag(
      "check-only",
      help = "if set, the generated files are checked against the existing files but are not written; exits 0 if the files match").orFalse


    val disable3rdPartyInRepos = Opts.flag(
      "disable-3rdparty-in-repo",
      help = "If set it controls if we should print out the 3rdparty source tree in the repo or not.").orFalse

    (repoRoot |@| depsFile |@| resolvedOutput |@| shaFile |@| targetFile |@| buildifier |@| pomFile |@| checkOnly |@| Verbosity.opt |@| disable3rdPartyInRepos).map(Generate(_, _, _, _, _, _, _, _, _, _))
  }

  case class FormatDeps(deps: Path, overwrite: Boolean, verbosity: Verbosity)
      extends Command
  val format = DCommand("format-deps", "format the dependencies yaml file") {
    val depsFile = Opts.option[Path](
      "deps",
      short = "d",
      help = "the ABSOLUTE path to your dependencies yaml file"
    )
    val overwrite = Opts
      .flag(
        "overwrite",
        short = "o",
        help = "if set, we overwrite the file after we read it"
      )
      .orFalse

    (depsFile |@| overwrite |@| Verbosity.opt).map(FormatDeps(_, _, _))
  }

  case class MergeDeps(
      deps: NonEmptyList[Path],
      output: Option[Path],
      verbosity: Verbosity
  ) extends Command
  val mergeDeps =
    DCommand("merge-deps", "merge a series of dependencies yaml file") {
      val deps = Opts.options[Path](
        "deps",
        short = "d",
        help = "list of ABSOLUTE paths of files to merge"
      )
      val out = Opts
        .option[Path]("output", short = "o", help = "merged output file")
        .orNone

      (deps |@| out |@| Verbosity.opt).map(MergeDeps(_, _, _))
    }

  implicit val langArg: Argument[Language] = new Argument[Language] {
    def defaultMetavar: String = "lang"
    def read(s: String) = s match {
      case "java"   => Validated.valid(Language.Java)
      case "kotlin" => Validated.valid(Language.Kotlin)
      case "scala"  => Validated.valid(Language.Scala.default)
      case other    => Validated.invalidNel(s"unknown language: $other")
    }
  }

  implicit val mvnArg: Argument[MavenCoordinate] =
    new Argument[MavenCoordinate] {
      def defaultMetavar: String = "maven-coord"
      def read(s: String) = MavenCoordinate.parse(s)
    }

  case class AddDep(
      deps: Path,
      lang: Language,
      coords: NonEmptyList[MavenCoordinate],
      verbosity: Verbosity
  ) extends Command
  val addDep = DCommand(
    "add-dep",
    "add dependencies (of a single language) to the yaml file"
  ) {
    val p = Opts.option[Path](
      "deps",
      short = "d",
      help = "the YAML dependency file to add to"
    )
    val lang = Opts.option[Language](
      "lang",
      short = "l",
      help = "the language of the given maven coordinate"
    )
    val mcs = Opts.arguments[MavenCoordinate]("mvn-coord")

    (p |@| lang |@| mcs |@| Verbosity.opt).map(AddDep(_, _, _, _))
  }

  val command: DCommand[Command] =
    DCommand(
      name = "bazel-deps",
      header =
        "a tool to manage transitive external Maven dependencies for bazel"
    ) {
      (Opts.help :: (List(generate, format, mergeDeps, addDep)
        .map(Opts.subcommand(_))))
        .reduce(_.orElse(_))
    }
}
