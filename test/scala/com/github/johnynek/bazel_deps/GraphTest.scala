package com.github.johnynek.bazel_deps

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers.check
import org.scalacheck.Prop.forAll
import org.scalacheck.{ Gen, Arbitrary }

class GraphTest extends FunSuite  {

  def graphGen[N, E](g: Gen[(N, Option[(N, E)])]): Gen[Graph[N, E]] = Gen.sized { s =>
    Gen.listOfN(s, g)
      .map { es =>
        es.foldLeft(Graph.empty[N, E]) {
          case (g, (n, None)) => g.addNode(n)
          case (g, (s, Some((d, e)))) => g.addEdge(Edge(s, d, e))
        }
      }
  }

  def edgeFrom[N, E](g: Graph[N, E]): Gen[Edge[N, E]] =
    Gen.oneOf(g.edgeIterator.toVector)

  def nodeFrom[N, E](g: Graph[N, E]): Gen[N] =
    Gen.oneOf(g.nodes.toVector)


  test("Graph tests") {
    val g = Graph.empty[Int, Unit]
    check(forAll { (i: Int) =>
      g.addEdge(Edge(i, i+1, ())).nodes(i)
    })

    assert(g.addEdge(Edge(1, 2, ())).nodes(1), "adding an edge adds the node")
    assert(g.addEdge(Edge(1, 2, ())).nodes(2), "adding an edge adds the node")
    assert(g.addEdge(Edge(1, 2, ())).removeNode(1).edges.size == 0, "removeNode 1")
    assert(g.addEdge(Edge(1, 2, ())).removeNode(1).nodes(2), "removeNode 1.nodes")
    assert(g.addEdge(Edge(1, 2, ())).removeNode(2).edges.size == 0, "removeNode 2")
    assert(g.addEdge(Edge(1, 2, ())).removeNode(2).nodes(1), "removeNode 1.nodes")
  }

  test("Sanity checks on generated graphs (non-dag)") {
    def nodeGen: Gen[Int] = Gen.choose(0, 1000)
    def genIntNode: Gen[(Int, Option[(Int, Unit)])] =
      for {
        src <- nodeGen
        coin <- Gen.oneOf(true, false)
        dest <- if (coin) nodeGen.map { d => Some((d, ())) } else Gen.const(None)
      } yield (src, dest)

    check(forAll(graphGen(genIntNode)) { g =>
      g.edges.iterator.flatMap { case (_, es) => es.iterator }.toSet == g.edgeIterator.toSet
    })
    check(forAll(graphGen(genIntNode), nodeGen) { (g, n) =>
      g.addNode(n).nodes(n)
    })
    check(forAll(graphGen(genIntNode), nodeGen) { (g, n) =>
      val newG = g.removeNode(n)
      (!newG.nodes(n)) && (!newG.edgeIterator.exists { case Edge(s, d, _) => (s == n) || (d == n) })
    })
    check(forAll(graphGen(genIntNode), nodeGen) { (g, n) =>
      val newG = g.removeNode(n)
      (!newG.nodes(n)) && (!newG.edgeIterator.exists { case Edge(s, d, _) => (s == n) || (d == n) })
    })
    check(forAll(graphGen(genIntNode), nodeGen, nodeGen) { (g, s, d) =>
      val newEdge = Edge(s, d, ())
      val newG = g.addEdge(newEdge)
      newG.nodes(s) &&
        newG.nodes(d) &&
        newG.hasSource(s)(newEdge) &&
        newG.hasDestination(d)(newEdge) &&
        newG.edgeIterator.toSet(newEdge)
    })
    // Check some removals:
    check(forAll(graphGen(genIntNode).flatMap { g => edgeFrom(g).map((g, _)) }) { case (g, e) =>
      val newG = g.removeEdge(e)
      !(newG.edgeIterator.exists(_ == e))
    })
    // Check some removals:
    check(forAll(graphGen(genIntNode).flatMap { g => nodeFrom(g).map((g, _)) }) { case (g, n) =>
      val newG = g.removeNode(n)
      newG.hasDestination(n).isEmpty &&
        newG.hasSource(n).isEmpty &&
        (!newG.nodes(n))
    })
  }
}
