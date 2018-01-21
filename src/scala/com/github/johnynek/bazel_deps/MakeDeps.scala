package com.github.johnynek.bazel_deps

import java.io.File
import java.nio.file.Paths

import io.circe.jawn.JawnParser

import scala.sys.process.{BasicIO, Process, ProcessIO}
import scala.util.{Failure, Success, Try}
import cats.instances.try_._
import com.github.johnynek.bazel_deps.IO.Path

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
    val projectRoot = g.repoRoot.toFile
    val resolverCachePath = model.getOptions.getResolverCache match {
      case ResolverCache.Local => Paths.get("target/local-repo")
      case ResolverCache.BazelOutputBase =>
        Try(Process(List("bazel", "info", "output_base"), projectRoot) !!) match {
          case Success(path) => Paths.get(path.trim, "bazel-deps/local-repo")
          case Failure(err) =>
            System.err.println(s"[ERROR]: Could not find resolver cache path -- `bazel info output_base` failed.\n$err")
            System.exit(1)
            sys.error("unreachable")
        }
    }
    val deps = model.dependencies
    val resolver = new Resolver(model.getOptions.getResolvers, resolverCachePath.toAbsolutePath)
    val graph = resolver.addAll(Graph.empty, deps.roots, model)
    // This is a defensive check that can be removed as we add more tests
    deps.roots.foreach { m => require(graph.nodes(m), s"$m") }

    Normalizer(graph, deps.roots, model.getOptions.getVersionConflictPolicy) match {
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
          .mapValues { ns =>
            ns.flatMap { n =>
              graph.hasDestination(n).filter(e => normalized.nodes(e.source))
            }
          }
          .filter { case (_, set) => set.map(_.destination.version).size > 1 }

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

        val formatter: Writer.BuildFileFormatter = g.buildifier match {
          // If buildifier is provided, run it with the unformatted contents on its stdin; it will print the formatted
          // result on stdout.
          case Some(buildifierPath) => (p, s) => {
            val output = new java.lang.StringBuilder()
            val error = new java.lang.StringBuilder()
            val processIO = new ProcessIO(
              os => {
                os.write(s.getBytes(IO.charset))
                os.close()
              },
              BasicIO.processFully(output),
              BasicIO.processFully(error)
            )
            val exit = Process(List(buildifierPath, "-path", p.asString, "-"), projectRoot).run(processIO).exitValue
            // Blocks until the process exits.
            if (exit != 0) {
              System.err.println(s"buildifier $buildifierPath failed (code $exit) for ${p.asString}:\n$error")
              System.exit(-1)
              sys.error("unreachable")
            }
            output.toString
          }
          // If no buildifier is provided, pass the contents through directly.
          case None => (_, s) => s
        }

        if (g.checkOnly) {
          executeCheckOnly(model, projectRoot, IO.path(workspacePath), ws, targets, formatter)
        } else {
          executeGenerate(model, projectRoot, IO.path(workspacePath), ws, targets, formatter)
        }
    }
  }

  private def executeCheckOnly(model: Model, projectRoot: File, workspacePath: IO.Path, workspaceContents: String, targets: List[Target], formatter: Writer.BuildFileFormatter): Unit = {
    // Build up the IO operations that need to run.
    val io = for {
      wsOK <- IO.compare(workspacePath, workspaceContents)
      wsbOK <- IO.compare(workspacePath.sibling("BUILD"), "")
      rootPath = Path(model.getOptions.getThirdPartyDirectory.parts)
      buildsOK <- Writer.compareBuildFiles(rootPath, model.getOptions.getBuildHeader, targets, formatter)
    } yield wsOK :: wsbOK :: buildsOK

    // Here we actually run the whole thing
    io.foldMap(IO.fileSystemExec(projectRoot)) match {
      case Failure(err) =>
        System.err.println(err)
        System.exit(-1)
      case Success(comparisons) =>
        val mismatchedFiles = comparisons.filter(!_.ok)
        if (mismatchedFiles.isEmpty) {
          println(s"all ${comparisons.size} generated files are up-to-date")
        } else {
          System.err.println(s"some generated files are not up-to-date:\n${mismatchedFiles.map(_.path.asString).sorted.mkString("\n")}")
          System.exit(2)
        }
    }
  }

  private def executeGenerate(model: Model, projectRoot: File, workspacePath: IO.Path, workspaceContents: String, targets: List[Target], formatter: Writer.BuildFileFormatter): Unit = {
    // Build up the IO operations that need to run. Till now,
    // nothing was written
    val io = for {
      _ <- IO.recursiveRmF(IO.Path(model.getOptions.getThirdPartyDirectory.parts))
      _ <- IO.mkdirs(workspacePath.parent)
      _ <- IO.writeUtf8(workspacePath, workspaceContents)
      _ <- IO.writeUtf8(workspacePath.sibling("BUILD"), "")
      rootPath = Path(model.getOptions.getThirdPartyDirectory.parts)
      builds <- Writer.createBuildFiles(rootPath, model.getOptions.getBuildHeader, targets, formatter)
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
