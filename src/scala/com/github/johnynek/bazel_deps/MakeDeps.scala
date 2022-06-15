package com.github.johnynek.bazel_deps

import java.io.File
import java.nio.file.{Path, Paths}
import io.circe.jawn.JawnParser
import org.slf4j.LoggerFactory

import scala.sys.process.{BasicIO, Process, ProcessIO}
import scala.util.{Failure, Success, Try}
import scala.collection.immutable.SortedMap
import cats.implicits._
import com.github.johnynek.bazel_deps.Writer.ArtifactEntry

object MakeDeps {

  private[this] val logger = LoggerFactory.getLogger("MakeDeps")

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
    val projectRoot = g.repoRoot.toFile

    resolverCachePath(model, projectRoot).flatMap(runResolve(model, _)) match {
      case Failure(err) =>
        logger.error("resolution and sha collection failed", err)
        System.exit(1)
      case Success((normalized, shas, duplicates)) =>
        // build the BUILDs in thirdParty
        val artifacts = Writer.artifactEntries(normalized, duplicates, shas, model) match {
          case Right(t) => t
          case Left(errs) =>
            errs.toList.foreach { e => logger.error(e.message) }
            System.exit(-1)
            sys.error("exited already")
        }

        executeGenerate(
          model,
          projectRoot,
          IO.path(g.resolvedOutput),
          artifacts
        )
    }
  }

  private def resolverCachePath(model: Model, projectRoot: File): Try[Path] =
    (model.getOptions.getResolverCache match {
      case ResolverCache.Local => Try(Paths.get("target/local-repo"))
      case ResolverCache.BazelOutputBase =>
        Try(
          Process(List("bazel", "info", "output_base"), projectRoot) !!
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
        val resolver = new AetherResolver(
          model.getOptions.getResolvers.collect { case e: MavenServer => e },
          resolverCachePath
        )
        resolver.run(resolve(model, resolver))
      case ResolverType.Coursier =>
        val ec = scala.concurrent.ExecutionContext.Implicits.global
        import scala.concurrent.duration._
        val resolver =
          new CoursierResolver(model.getOptions.getResolvers, ec, 3600.seconds)
        resolver.run(resolve(model, resolver))
      case g: ResolverType.Gradle =>
        val ec = scala.concurrent.ExecutionContext.Implicits.global
        import scala.concurrent.duration._
        val resolver =
          new GradleResolver(model.getOptions.getResolvers, ec, 3600.seconds, g)
        resolver.run(resolver.resolve(model))
    }

  private def resolve[F[_]](model: Model, resolver: Resolver[F]): F[
    (
        Graph[MavenCoordinate, Unit],
        SortedMap[MavenCoordinate, ResolvedShasValue],
        Map[UnversionedCoordinate, Set[Edge[MavenCoordinate, Unit]]]
    )
  ] = {
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

  case class AllArtifacts(artifact: List[ArtifactEntry])
  private def executeGenerate(
      model: Model,
      projectRoot: File,
      resolvedJsonOutputPath: IO.Path,
      artifacts: List[ArtifactEntry]
  ): Unit = {
    import _root_.io.circe.syntax._
    import _root_.io.circe.generic.auto._

    val io = for {
      b <- IO.exists(resolvedJsonOutputPath.parent)
      _ <- if (b) IO.const(false) else IO.mkdirs(resolvedJsonOutputPath.parent)
      allArtifacts = AllArtifacts(artifacts.sortBy(_.artifact))
      artifactsJson = allArtifacts.asJson
      _ <- IO.writeUtf8(resolvedJsonOutputPath, artifactsJson.spaces2)
    } yield ()

    // Here we actually run the whole thing
    io.foldMap(IO.fileSystemExec(projectRoot)) match {
      case Failure(err) =>
        logger.error("Failure during IO:", err)
        System.exit(-1)
      case Success(builds) =>
        println(s"wrote ${artifacts.size} targets")
    }
  }
}
