package com.github.johnynek.bazel_deps

import org.scalatest.funsuite.AnyFunSuite

class GraphTest extends AnyFunSuite {
  test("Graph tests") {
    val g = Graph.empty[Int, Unit]

    assert(g.addEdge(Edge(1, 2, ())).nodes(1), "adding an edge adds the node")
    assert(g.addEdge(Edge(1, 2, ())).nodes(2), "adding an edge adds the node")
    assert(
      g.addEdge(Edge(1, 2, ())).removeNode(1).edges.size == 0,
      "removeNode 1"
    )
    assert(
      g.addEdge(Edge(1, 2, ())).removeNode(1).nodes(2),
      "removeNode 1.nodes"
    )
    assert(
      g.addEdge(Edge(1, 2, ())).removeNode(2).edges.size == 0,
      "removeNode 2"
    )
    assert(
      g.addEdge(Edge(1, 2, ())).removeNode(2).nodes(1),
      "removeNode 1.nodes"
    )
  }
}
