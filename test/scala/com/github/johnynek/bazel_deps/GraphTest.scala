package com.github.johnynek.bazel_deps

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers.check
import org.scalacheck.Prop.forAll

class GraphTest extends FunSuite  {
  test("Graph tests") {
    val g = Graph.empty[Int, Unit]
    check(forAll { (i: Int) =>
      g.addEdge(Edge(i, i+1, ())).nodes(i)
    })

    assert(g.addEdge(Edge(1, 2, ())).nodes(1), "adding an edge adds the node")
    assert(g.addEdge(Edge(1, 2, ())).nodes(2), "adding an edge adds the node")
  }
}
