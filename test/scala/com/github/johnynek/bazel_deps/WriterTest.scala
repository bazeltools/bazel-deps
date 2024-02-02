package com.github.johnynek.bazel_deps

import cats.data.NonEmptyList
import org.scalatest.funsuite.AnyFunSuite

class WriterTest extends AnyFunSuite {

  def graph(edges: (String, String)*): Graph[MavenCoordinate, Unit] =
    edges.foldLeft(Graph.empty[MavenCoordinate, Unit]) { case (g, (s, d)) =>
      g.addEdge(Edge(MavenCoordinate(s), MavenCoordinate(d), ()))  
    }

  test("empty targets") {
    val targs = Writer.targets(graph(), Model.empty)
    assert(targs == Right(Nil))
  }

  test("a circular dep gives the errors") {
    val g = graph(
      "foo:foo_a:1" -> "foo:foo_b:1",
      "foo:foo_b:1" -> "foo:foo_c:1",
      "foo:foo_c:1" -> "foo:foo_a:1",
      "foo:foo_d:1" -> "foo:foo_a:1",
    )

    val targs =
    Writer.targets(g, Model.empty) match {
      case Left(NonEmptyList(ce @ Writer.TargetsError.CircularExports(_, _, _, _, _), Nil)) =>
        assert(ce.loopNodes == Set(MavenCoordinate("foo:foo_c:1")))
        // if we cut the loop edges we are okay
        val good = ce.loopEdges.foldLeft(g)(_.removeEdge(_))
        assert(Writer.targets(good, Model.empty).isRight)
      case unexpected => fail(unexpected.toString)
    }
  }
}