package com.github.johnynek.bazel_deps

import java.io.File
import cats.data.Xor

trait MakeDeps {
  /**
   * the second item is a relative path to the workspace.bzl file
   * we will create. The third item is an absolute path to the root
   * of the directory
   */
  def getSettings(args: Array[String]): (Model, List[String], File)

  def main(args: Array[String]): Unit = {
    val (model, workspacePath, projectRoot) = getSettings(args)
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
          _ <- IO.recursiveRm(IO.Path(model.getOptions.getThirdPartyDirectory.parts))
          _ <- IO.writeUtf8(IO.Path(workspacePath), ws)
          builds <- Writer.createBuildFiles(model.getOptions.getBuildHeader, targets)
        } yield builds

        // Here we actually run the whole thing
        io.foldMap(IO.fileSystemExec(projectRoot)) match {
          case Xor.Left(err) =>
            System.err.println(err)
            System.exit(-1)
          case Xor.Right(builds) =>
            println(s"wrote ${targets.size} targets in $builds BUILD files")
        }
    }
  }
}
