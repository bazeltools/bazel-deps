package com.github.johnynek.bazel_deps

import java.io.File

import cats.implicits._
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object GeneratePom {
  def readModel(path: File): Either[String, Model] = {
    val content: Either[String, String] =
      Model.readFile(path) match {
        case Success(str) => Right(str)
        case Failure(err) =>
          Left(s"[ERROR]: Failed to read ${path}.\n$err")
      }

    content.right.flatMap { c =>
      Decoders.decodeModel(Yaml, c) match {
        case Right(m) => Right(m)
        case Left(err) => Left(s"[ERROR]: Failed to parse ${path}.\n$err")
      }
    }
  }

  private def resolve[F[_]](model: Model, resolver: Resolver[F]): F[(Graph[MavenCoordinate, Unit])] = {
    import resolver.resolverMonad

    val deps = model.dependencies
    resolver.buildGraph(deps.roots.toList.sorted, model)
      .flatMap { graph =>
        // This is a defensive check that can be removed as we add more tests
        resolverMonad.catchNonFatal {
          deps.roots.foreach { m => require(graph.nodes(m), s"$m") }
          graph
        }
      }
      .flatMap { graph =>
        Normalizer(graph, deps.roots, model.getOptions.getVersionConflictPolicy) match {
          case None =>
            val output = graph.nodes.groupBy(_.unversioned)
              .mapValues { _.map(_.version).toList.sorted }
              .filter { case (_, s) => s.lengthCompare(1) > 0 }
              .map { case (u, vs) => s"""${u.asString}: ${vs.mkString(", ")}\n""" }
              .mkString("\n")
            resolverMonad.raiseError(new Exception(
              s"could not normalize versions:\n$output"))
          case Some(normalized) =>

            /**
             * Make sure all the optional versioned items were found
             */
            val uvNodes = normalized.nodes.map(_.unversioned)
            val check = deps.unversionedRoots.filterNot { u =>
              uvNodes(u) || model.getReplacements.get(u).isDefined
            }.toList match {
              case Nil => resolverMonad.pure(())
              case missing =>
                val output = missing.map(_.asString).mkString(" ")
                resolverMonad.raiseError(new Exception(s"Missing unversioned deps in the normalized graph: $output"))
            }

            for {
              _ <- check
            } yield (normalized)
        }
      }
  }

  def apply(deps: File, pomFile: File, groupId: String, artifactId: String, version: String): Unit = {
    readModel(deps) match {
      case Left(msg) => System.err.println(msg)
      case Right(model) =>
        val ec = scala.concurrent.ExecutionContext.Implicits.global
        val resolver = new CoursierResolver(model.getOptions.getResolvers, ec, 3600.seconds)
        val mavenCoordinateXml = resolver.run(resolve(model, resolver)) match {
          case Failure(err) =>
            Left(s"[ERROR]: Resolution failed ${err}.\n$err")
          case Success((normalized)) =>
            normalized.nodes.toList.map {
              mv =>
                mv.toXml
            }
        }

        val pomXml = <project>
          <modelVersion>4.0.0</modelVersion>

          <groupId>{groupId}</groupId>
          <artifactId>{artifactId}</artifactId>
          <version>{version}</version>

          <dependencies>
            {mavenCoordinateXml}
          </dependencies>
        </project>

        val p = new scala.xml.PrettyPrinter(80, 2)
        scala.xml.XML.save(
          pomFile.toString,
          scala.xml.XML.loadString(p.format(pomXml)),
          "UTF-8",
          true
        )
    }
  }
}
