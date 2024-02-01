package com.github.johnynek.bazel_deps

import io.circe.jawn.JawnParser
import java.nio.file.Paths
import org.scalatest.funsuite.AnyFunSuite
import scala.util.{Success, Failure}

import Decoders._

class GradleResolverTest extends AnyFunSuite {
 
  def resolverType(str: String): ResolverType.Gradle =
    (new JawnParser).decode[Options](str) match {
      case Right(o) => o.resolverType match {
        case Some(r) => r.asInstanceOf[ResolverType.Gradle]
        case None => ResolverType.Gradle.empty
      }
      case Left(error) => sys.error(s"Failed to decode $str: ${error}")
    }

  def deps(str: String): Map[String, GradleLockDependency] =
    (new JawnParser).decode[Map[String, GradleLockDependency]](str) match {
      case Right(t) => t
      case Left(error) => sys.error(s"Failed to decode $str: ${error}")
    }

  def resolver(resType: String): GradleResolver =
    new GradleResolver(
      Paths.get(""),
      VersionConflictPolicy.Highest,
      resolverType(resType),
      arg => Failure(new Exception(s"tried to resolve: $arg"))
    )

  def assertMatch(gradleOpts: String, gradleDeps: String, graph: Graph[MavenCoordinate, Unit]) = {
    val graph = resolver(gradleOpts).buildGraphFromDepMap(deps(gradleDeps))
    val maybeShow = graph.fold(e => e.toString, _.show(_.asString))
    assert(graph == Success(Graph.empty), s"rendered:\n\n$maybeShow")
  } 

  def mvn(str: String): MavenCoordinate =
    MavenCoordinate(str)

  test("empty test") {
    assertMatch("""{
    }""",
    """{

    }""",
    Graph.empty)
  }

  test("basic 1") {
    assertMatch("""{
      "resolverType": "gradle",
      "ignoreDependencyEdge": ["foo:f", "bar:b"]
    }""",
    """{
    "foo:f": {
      "transitive": ["bar:b"]
    },
    "bar:b": {
      "transitive": ["foo:f"]
    }
    }""",
    Graph.empty.addEdge(Edge(mvn("bar:b:"), mvn("foo:f:"), ())))
  }
}