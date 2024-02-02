package com.github.johnynek.bazel_deps

import io.circe.jawn.JawnParser
import java.nio.file.Paths
import org.scalatest.funsuite.AnyFunSuite
import scala.util.{Success, Failure}
import scala.collection.immutable.SortedMap

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

  def deps(str: String): SortedMap[String, GradleLockDependency] =
    (new JawnParser).decode[SortedMap[String, GradleLockDependency]](str) match {
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
    val rgraph = resolver(gradleOpts).buildGraphFromDepMap(deps(gradleDeps))
    val maybeShow = rgraph.fold(e => e.toString, _.show(_.asString))
    assert(rgraph == Success(graph), s"rendered:\n\n$maybeShow")
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

  test("resolver parses the ignores") {
    val tpe = resolverType("""{
      "resolverType": "gradle",
      "resolverOptions": {
        "ignoreDependencyEdge": [["foo:f", "bar:b"]]
      }
    }""")

    assert(tpe.ignoreDependencyEdge == Some(Set(("foo:f", "bar:b"))))
  }

  test("resolver parses the ignores solr") {
    val res = resolver("""{
      "resolverType": "gradle",
      "resolverOptions": {
        "ignoreDependencyEdge": [
          [ "netflix:account-metadata", "netflix:custeng-subscriber-model" ],
          [ "org.apache.solr:solr-solrj", "org.apache.solr:solr-solrj-streaming" ],
          [ "org.apache.solr:solr-solrj-streaming", "org.apache.solr:solr-solrj" ],
          [ "org.apache.solr:solr-solrj", "org.apache.solr:solr-solrj-zookeeper" ],
          [ "org.apache.solr:solr-solrj-zookeeper", "org.apache.solr:solr-solrj" ]
        ]
      }
    }""")
    val tpe = res.resolverType

    assert(tpe.ignoreDependencyEdge == Some(Set(
      ( "netflix:account-metadata", "netflix:custeng-subscriber-model" ),
      ( "org.apache.solr:solr-solrj", "org.apache.solr:solr-solrj-streaming" ),
      ( "org.apache.solr:solr-solrj-streaming", "org.apache.solr:solr-solrj" ),
      ( "org.apache.solr:solr-solrj", "org.apache.solr:solr-solrj-zookeeper" ),
      ( "org.apache.solr:solr-solrj-zookeeper", "org.apache.solr:solr-solrj" )
    )))

    // example we found in maven
    val solr = "org.apache.solr:solr-solrj:9.4.1"
    val streaming = "org.apache.solr:solr-solrj-streaming:9.4.1"
    val zk = "org.apache.solr:solr-solrj-zookeeper:9.4.1"
    assert(res.ignoreEdge(mvn(solr), mvn(streaming)))
    assert(res.ignoreEdge(mvn(streaming), mvn(solr)))
    assert(res.ignoreEdge(mvn(solr), mvn(zk)))
    assert(res.ignoreEdge(mvn(zk), mvn(solr)))
  }

  test("basic 1") {
    assertMatch("""{
      "resolverType": "gradle",
      "resolverOptions": {
        "ignoreDependencyEdge": [["foo:f", "bar:b"]]
      }
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

  test("basic 1: versioned") {
    assertMatch("""{
      "resolverType": "gradle",
      "resolverOptions": {
        "ignoreDependencyEdge": [["foo:f", "bar:b"]]
      }
    }""",
    """{
    "foo:f": {
      "locked": "1.0",
      "transitive": ["bar:b"]
    },
    "bar:b": {
      "locked": "2.0",
      "transitive": ["foo:f"]
    }
    }""",
    Graph.empty.addEdge(Edge(mvn("bar:b:2.0"), mvn("foo:f:1.0"), ())))
  }
}