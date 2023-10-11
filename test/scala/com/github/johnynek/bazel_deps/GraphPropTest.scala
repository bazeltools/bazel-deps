package com.github.johnynek.bazel_deps

import org.scalatest.propspec.AnyPropSpec
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Arbitrary}

class GraphPropTest extends AnyPropSpec {

  def graphGen[N, E](g: Gen[(N, Option[(N, E)])]): Gen[Graph[N, E]] =
    Gen.sized { s =>
      Gen
        .listOfN(s, g)
        .map { es =>
          es.foldLeft(Graph.empty[N, E]) {
            case (g, (n, None))         => g.addNode(n)
            case (g, (s, Some((d, e)))) => g.addEdge(Edge(s, d, e))
          }
        }
    }

  def edgeFrom[N, E](g: Graph[N, E]): Gen[Option[Edge[N, E]]] = {
    val es = g.edgeIterator.toVector
    if (es.isEmpty) Gen.const(None)
    else Gen.oneOf(es).map(Some(_))
  }

  def nodeFrom[N, E](g: Graph[N, E]): Gen[Option[N]] =
    if (g.nodes.isEmpty) Gen.const(None)
    else Gen.oneOf(g.nodes.toVector).map(Some(_))

  def randomWalkDest[N, E](g: Graph[N, E]): Option[Gen[(N, N)]] =
    if (g.nodes.isEmpty) None
    else
      Some(Gen.choose(0, g.nodes.size).flatMap { hops =>
        def step(hop: Int, n: N): Gen[N] =
          if (hop <= 0) Gen.const(n)
          else {
            val nexts = g.hasSource(n).toVector.map(_.destination)
            if (nexts.isEmpty) Gen.const(n)
            else Gen.oneOf(nexts).flatMap(step(hop - 1, _))
          }

        for {
          st <- Gen.oneOf(g.nodes.toVector)
          end <- step(hops, st)
        } yield (st, end)
      })

  def nodeGen: Gen[Int] = Gen.choose(0, 1000)
  def genIntNode: Gen[(Int, Option[(Int, Unit)])] =
    for {
      src <- nodeGen
      coin <- Gen.oneOf(true, false)
      dest <-
        if (coin) nodeGen.map { d => Some((d, ())) }
        else Gen.const(None)
    } yield (src, dest)

  property("Add edge") {
    val g = Graph.empty[Int, Unit]
    forAll { (i: Int) =>
      g.addEdge(Edge(i, i + 1, ())).nodes(i)
    }
  }

  property("Sanity checks on generated graphs (non-dag)") {
    forAll(graphGen(genIntNode)) { g =>
      g.edges.iterator.flatMap { case (_, es) =>
        es.iterator
      }.toSet == g.edgeIterator.toSet
    }
  }

  property("Sanity checks on generated graphs (non-dag) 2") {
    forAll(graphGen(genIntNode), nodeGen) { (g, n) =>
      g.addNode(n).nodes(n)
    }
  }

  property("Sanity checks on generated graphs (non-dag) 3") {
    forAll(graphGen(genIntNode), nodeGen) { (g, n) =>
      val newG = g.removeNode(n)
      (!newG.nodes(n)) && (!newG.edgeIterator.exists { case Edge(s, d, _) =>
        (s == n) || (d == n)
      })
    }
  }

  property("Sanity checks on generated graphs (non-dag) 4") {
    forAll(graphGen(genIntNode), nodeGen) { (g, n) =>
      val newG = g.removeNode(n)
      (!newG.nodes(n)) && (!newG.edgeIterator.exists { case Edge(s, d, _) =>
        (s == n) || (d == n)
      })
    }
  }

  property("Sanity checks on generated graphs (non-dag) 5") {
    forAll(graphGen(genIntNode), nodeGen, nodeGen) { (g, s, d) =>
      val newEdge = Edge(s, d, ())
      val newG = g.addEdge(newEdge)
      newG.nodes(s) &&
      newG.nodes(d) &&
      newG.hasSource(s)(newEdge) &&
      newG.hasDestination(d)(newEdge) &&
      newG.edgeIterator.toSet(newEdge)
    }
  }

  property("Check removals") {
    // Check some removals:
    forAll(graphGen(genIntNode).flatMap { g =>
      edgeFrom(g).map((g, _))
    }) {
      case (g, Some(e)) =>
        val newG = g.removeEdge(e)
        !(newG.edgeIterator.exists(_ == e))
      case (g, None) => true
    }
  }

  property("Check removals 2") {
    // Check some removals:
    forAll(graphGen(genIntNode).flatMap { g =>
      nodeFrom(g).map((g, _))
    }) {
      case (g, Some(n)) =>
        val newG = g.removeNode(n)
        newG.hasDestination(n).isEmpty &&
        newG.hasSource(n).isEmpty &&
        (!newG.nodes(n))
      case (g, None) => true
    }
  }

  property("randomwalk is in reflexiveTransitiveClosure") {
    val genEnds = for {
      g <- graphGen(genIntNode)
      optPair <- randomWalkDest(g).fold(Gen.const(Option.empty[(Int, Int)]))(
        _.map(Some(_))
      )
    } yield (g, optPair)

    forAll(genEnds) {
      case (g, Some((s, e))) => g.reflexiveTransitiveClosure(List(s))(e)
      case (g, None)         => true
    }
  }
}
