package com.github.johnynek.bazel_deps

import java.io.File
import io.circe.jawn.JawnParser
import scala.util.{ Failure, Success }
import cats.instances.try_._

object MakeDeps {
  def apply(g: Command.Generate): Unit = {

    val content: String = Model.readFile(g.absDepsFile) match {
      case Success(str) => str
      case Failure(err) =>
        System.err.println(s"[ERROR]: Failed to read ${g.depsFile}.\n$err")
        System.exit(1)
        sys.error("unreachable")
    }

    val parser = if (g.depsFile.endsWith(".json")) new JawnParser else Yaml
    val model = Decoders.decodeModel(parser, content) match {
      case Right(m) => m
      case Left(err) =>
        System.err.println(s"[ERROR]: Failed to parse ${g.depsFile}.\n$err")
        System.exit(1)
        sys.error("unreachable")
    }
    val workspacePath = g.shaFilePath
    val projectRoot = g.repoRoot
    val deps = model.dependencies
    val resolver = new Resolver(model.getOptions.getResolvers)
    val graph = resolver.addAll(Graph.empty, deps.roots, model)
    // This is a defensive check that can be removed as we add more tests
    deps.roots.foreach { m => require(graph.nodes(m), s"$m") }

    Normalizer(graph, deps, model.getOptions) match {
      case None =>
        println("[ERROR] could not normalize versions:")
        println(graph.nodes.groupBy(_.unversioned)
          .mapValues { _.map(_.version).toList.sorted }
          .filter { case (_, s) => s.lengthCompare(1) > 0 }
          .map { case (u, vs) => s"""${u.asString}: ${vs.mkString(", ")}\n""" }
          .mkString("\n"))
        System.exit(1)
      case Some(normalized) =>
        /**
         * The graph is now normalized, lets get the shas
         */
        val duplicates = graph.nodes
          .groupBy(_.unversioned)
          .mapValues(_.map(_.version))
          .filter { case (_, set) => set.size > 1 }

        /**
         * Make sure all the optional versioned items were found
         */
        val uvNodes = normalized.nodes.map(_.unversioned)
        deps.unversionedRoots.filterNot { u =>
            uvNodes(u) || model.getReplacements.get(u).isDefined
          }.toList match {
            case Nil => ()
            case missing =>
              System.err.println(
                s"Missing unversioned deps in the normalized graph: ${missing.map(_.asString).mkString(" ")}")
              System.exit(-1)
          }

        def replaced(m: MavenCoordinate): Boolean =
          model.getReplacements.get(m.unversioned).isDefined

        val shas = resolver.getShas(normalized.nodes.filterNot(replaced))
        // build the workspace
        def ws = Writer.workspace(normalized, duplicates, shas, model)
        // build the BUILDs in thirdParty
        val targets = Writer.targets(normalized, model) match {
          case Right(t) => t
          case Left(err) =>
            System.err.println(s"""Could not find explicit exports named by: ${err.mkString(", ")}""")
            System.exit(-1)
            sys.error("exited already")
        }
        // Build up the IO operations that need to run. Till now,
        // nothing was written
        val io = for {
          _ <- IO.recursiveRmF(IO.Path(model.getOptions.getThirdPartyDirectory.parts))
          wsp = IO.Path(workspacePath)
          _ <- IO.mkdirs(wsp.parent)
          _ <- IO.writeUtf8(wsp, ws)
          builds <- Writer.createBuildFiles(model.getOptions.getBuildHeader, targets)
        } yield builds

        // Here we actually run the whole thing
        io.foldMap(IO.fileSystemExec(projectRoot)) match {
          case Failure(err) =>
            System.err.println(err)
            System.exit(-1)
          case Success(builds) =>
            println(s"wrote ${targets.size} targets in $builds BUILD files")
        }
    }
  }
}
