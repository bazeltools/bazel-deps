package com.github.johnynek.bazel_deps

import coursier.util.Task
import io.circe.jawn.JawnParser
import java.nio.file.{Path, Paths}
import org.slf4j.{Logger, LoggerFactory}
import scala.collection.immutable.SortedMap
import scala.sys.process.{BasicIO, Process, ProcessIO}
import scala.util.{Failure, Success, Try}

import cats.implicits._

object MakeDeps {

  private[this] val logger: Logger = LoggerFactory.getLogger("MakeDeps")

  def apply(g: Command.Generate): Unit = {

    val content: String = Model.readFile(g.absDepsFile) match {
      case Success(str) => str
      case Failure(err) =>
        logger.error(s"Failed to read ${g.depsFile}", err)
        System.exit(1)
        sys.error("unreachable")
    }

    val parser = if (g.depsFile.endsWith(".json")) new JawnParser else Yaml
    val model = Decoders.decodeModel(parser, content) match {
      case Right(m) => m
      case Left(err) =>
        logger.error(s"Failed to parse ${g.depsFile}.", err)
        System.exit(1)
        sys.error("unreachable")
    }
    val projectRoot = g.repoRoot

    resolverCachePath(model, projectRoot).flatMap(runResolve(projectRoot, model, _)) match {
      case Failure(err) =>
        logger.error("resolution and sha collection failed", err)
        System.exit(1)
      case Success((normalized, shas, duplicates)) =>
        // build the BUILDs in thirdParty
        val targets = Writer.targets(normalized, model) match {
          case Right(t) => t
          case Left(errs) =>
            errs.toList.foreach { e => logger.error(e.message) }
            System.exit(-1)
            sys.error("exited already")
        }
        val formatter: Writer.BuildFileFormatter = g.buildifier match {
          // If buildifier is provided, run it with the unformatted contents on its stdin; it will print the formatted
          // result on stdout.
          case Some(buildifierPath) => (p, s) => {
            val output = new java.lang.StringBuilder()
            val error = new java.lang.StringBuilder()
            val processIO = new ProcessIO(
              os => {
                os.write(s.getBytes(FS.charset))
                os.close()
              },
              BasicIO.processFully(output),
              BasicIO.processFully(error)
            )
            val exit = Process(List(buildifierPath, "-path", p.asString, "-"), projectRoot.toFile).run(processIO).exitValue
            // Blocks until the process exits.
            if (exit != 0) {
              logger.error(s"buildifier $buildifierPath failed (code $exit) for ${p.asString}:\n$error")
              System.exit(-1)
              sys.error("unreachable")
            }
            output.toString
          }
          // If no buildifier is provided, pass the contents through directly.
          case None => (_, s) => s
        }

        // build the BUILDs in thirdParty
        val artifacts =
          Writer.artifactEntries(normalized, duplicates, shas, model) match {
            case Right(t) => t
            case Left(errs) =>
              errs.toList.foreach { e => logger.error(e.message) }
              System.exit(-1)
              sys.error("exited already")
          }

        // build the workspace
        val ws = Writer.workspace(g.depsFile, normalized, duplicates, shas, model)
        // creates pom xml when path is provided
        val pomIO = FS.orUnit(g.pomFile.map { fileName => CreatePom.writeIO(normalized, FS.path(fileName)) })
        val io = executeGenerate(
          model,
          g.shaFilePath.map(FS.path(_)),
          g.targetFile.map(FS.path(_)),
          g.enable3rdPartyInRepo,
          ws,
          targets,
          formatter,
          g.resolvedOutput.map(FS.path),
          artifacts) >> pomIO

        if (g.checkOnly) {
          val check = new FS.ReadCheckExec(projectRoot)
          io.foldMap(check) match {
            case Failure(err) =>
              logger.error("Failure during IO:", err)
              val errs = check.logErrorCount { ce => logger.error(ce.message) }
              logger.error(s"found $errs errors.")
              System.exit(-1)
            case Success(_) =>
              println(s"checked ${artifacts.size} targets")
              val errs = check.logErrorCount { ce => logger.error(ce.message) }
              if (errs != 0) {
                logger.error(s"found $errs errors.")
                System.exit(2) // this error got assigned error 2 somehow
              }
          }
        }
        else {
          val exec = new FS.ReadWriteExec(projectRoot)
          // Here we actually run the whole thing
          io.foldMap(exec) match {
            case Failure(err) =>
              logger.error("Failure during IO:", err)
              System.exit(-1)
            case Success(_) =>
              println(s"wrote ${artifacts.size} targets")
          }
        }
      }
  }

  private def resolverCachePath(model: Model, projectRoot: Path): Try[Path] =
    (model.getOptions.getResolverCache match {
      case ResolverCache.Local => Try(Paths.get("target/local-repo"))
      case ResolverCache.BazelOutputBase =>
        Try(
          Process(List("bazel", "info", "output_base"), projectRoot.toFile) !!
        ) match {
          case Success(path) =>
            Try(Paths.get(path.trim, "bazel-deps/local-repo"))
          case Failure(err) =>
            logger.error(
              s"Could not find resolver cache path -- `bazel info output_base` failed.",
              err
            )
            Failure(err)
        }
    })
      .map(_.toAbsolutePath)

  private[bazel_deps] def runResolve(
      rootPath: Path,
      model: Model,
      resolverCachePath: Path
  ): Try[
    (
        Graph[MavenCoordinate, Unit],
        SortedMap[MavenCoordinate, ResolvedShasValue],
        Map[UnversionedCoordinate, Set[Edge[MavenCoordinate, Unit]]]
    )
  ] =
    model.getOptions.getResolverType match {
      case ResolverType.Aether =>
        resolve(
          model,
          new AetherResolver(
            model.getOptions.getResolvers.collect { case e: MavenServer => e },
            resolverCachePath)
        )
      case ResolverType.Coursier =>
        val ec = scala.concurrent.ExecutionContext.Implicits.global
        import scala.concurrent.duration._
        resolve(
          model,
          new CoursierResolver(model.getOptions.getResolvers, ec, 3600.seconds, resolverCachePath)
        )
      case g: ResolverType.Gradle =>
        val ec = scala.concurrent.ExecutionContext.Implicits.global
        import scala.concurrent.duration._

        lazy val coursierResolver =
          new CoursierResolver(model.getOptions.getResolvers, ec, 3600.seconds, resolverCachePath)

        val resolver = new GradleResolver(
            rootPath,
            model.getOptions.getVersionConflictPolicy,
            g,
            { ls => coursierResolver.run(coursierResolver.getShas(ls)) })
        // Note: this branch fully defers to GradleResolver and not
        // the private resolve(model, resolver) method here
        resolver.resolve(model)
    }

  private type Res = (
        Graph[MavenCoordinate, Unit],
        SortedMap[MavenCoordinate, ResolvedShasValue],
        Map[UnversionedCoordinate, Set[Edge[MavenCoordinate, Unit]]]
    )

  private def resolve[F[_]](model: Model, resolver: Resolver[F]): Try[Res] = 
  resolver.run[Res] {
    import resolver.resolverMonad

    val deps = model.dependencies

    resolver
      .buildGraph(deps.roots.toList.sorted, model)
      .flatMap { graph =>
        // This is a defensive check that can be removed as we add more tests
        resolverMonad.catchNonFatal {
          deps.roots.foreach { m => require(graph.nodes(m), s"$m") }
          graph
        }
      }
      .flatMap { graph =>
        Normalizer(
          graph,
          deps.roots,
          model.getOptions.getVersionConflictPolicy
        ) match {
          case None =>
            val output = graph.nodes
              .groupBy(_.unversioned)
              .mapValues { _.map(_.version).toList.sorted }
              .filter { case (_, s) => s.lengthCompare(1) > 0 }
              .map { case (u, vs) =>
                s"""${u.asString}: ${vs.mkString(", ")}\n"""
              }
              .mkString("\n")
            resolverMonad.raiseError(
              new Exception(s"could not normalize versions:\n$output")
            )
          case Some(normalized) =>
            /** The graph is now normalized, lets get the shas
              */
            val duplicates = graph.nodes
              .groupBy(_.unversioned)
              .mapValues { ns =>
                ns.flatMap { n =>
                  graph
                    .hasDestination(n)
                    .filter(e => normalized.nodes(e.source))
                }
              }
              .filter { case (_, set) =>
                set.map(_.destination.version).size > 1
              }

            /** Make sure all the optional versioned items were found
              */
            val uvNodes = normalized.nodes.map(_.unversioned)
            val check = deps.unversionedRoots.filterNot { u =>
              uvNodes(u) || model.getReplacements.get(u).isDefined
            }.toList match {
              case Nil => resolverMonad.pure(())
              case missing =>
                val output = missing.map(_.asString).mkString(" ")
                resolverMonad.raiseError(
                  new Exception(
                    s"Missing unversioned deps in the normalized graph: $output"
                  )
                )
            }

            def replaced(m: MavenCoordinate): Boolean =
              model.getReplacements.get(m.unversioned).isDefined

            for {
              _ <- check
              shas <- resolver.getShas(
                normalized.nodes.filterNot(replaced).toList.sorted
              )
            } yield (normalized, shas, duplicates)
        }
      }
  }

  case class AllArtifacts(artifacts: List[ArtifactEntry])

  private def executeGenerate(
      model: Model,
      workspacePath: Option[FS.Path],
      targetFileOpt: Option[FS.Path],
      enable3rdPartyInRepo: Boolean,
      workspaceContents: String,
      targets: List[Target],
      formatter: Writer.BuildFileFormatter,
      resolvedJsonOutputPathOption: Option[FS.Path],
      artifacts: List[ArtifactEntry]
  ): FS.Result[Unit] = {
    import _root_.io.circe.syntax._
    import _root_.io.circe.generic.auto._

    val buildFileName = model.getOptions.getBuildFileName

    for {
      _ <- FS.orUnit(workspacePath.map { wp =>
        for {
          originalBuildFile <- FS.readUtf8(wp.sibling(buildFileName))
          _ <- FS.mkdirs(wp.parent)
          _ <- FS.writeUtf8(wp, workspaceContents)
          _ <- FS.writeUtf8(wp.sibling(buildFileName), originalBuildFile.getOrElse(""))
        } yield ()
      })
      // If the 3rdparty directory is empty we shouldn't wipe out the current working directory.
      _ <- if (enable3rdPartyInRepo && model.getOptions.getThirdPartyDirectory.parts.nonEmpty) FS.recursiveRmF(FS.Path(model.getOptions.getThirdPartyDirectory.parts), false) else FS.const(0)
      _ <- Writer.createBuildFilesAndTargetFile(model.getOptions.getBuildHeader, targets, targetFileOpt, enable3rdPartyInRepo, model.getOptions.getThirdPartyDirectory, formatter, buildFileName)
      _ <- FS.orUnit(resolvedJsonOutputPathOption.map { resolvedJsonOutputPath =>
        for {
          b <- FS.exists(resolvedJsonOutputPath.parent)
          _ <- if (b) FS.const(false) else FS.mkdirs(resolvedJsonOutputPath.parent)
          allArtifacts = AllArtifacts(artifacts.sortBy(_.artifact))
          artifactsJson = allArtifacts.asJson.spaces2
          _ <- if (resolvedJsonOutputPath.extension.endsWith(".gz")) {
            FS.writeGzipUtf8(resolvedJsonOutputPath, artifactsJson)
          } else {
            FS.writeUtf8(resolvedJsonOutputPath, artifactsJson)
          }
        } yield ()
      })
    } yield ()
  }
}
