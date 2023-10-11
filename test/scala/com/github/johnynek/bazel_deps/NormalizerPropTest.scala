package com.github.johnynek.bazel_deps

import cats.implicits._
import cats.{Foldable, Monad}
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.prop.Configuration.PropertyCheckConfiguration
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.{Gen, Arbitrary}

class NormalizerPropTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("property") {
    val graphWithRoots = MavenGraphGen.genMavenGraph.flatMap { graph =>
      val allNodes = graph.nodes.toList
      Gen
        .choose(0, allNodes.size)
        .flatMap(Gen.pick(_, allNodes))
        .map((graph, _))
    }
    implicit val generatorDrivenConfig =
      PropertyCheckConfiguration(minSuccessful = 2000)

    forAll(graphWithRoots) { case (g, roots) =>
      Normalizer(g, roots.toSet, VersionConflictPolicy.Highest) match {
        case None    => fail(s"couldn't normalize $g")
        case Some(g) =>
          // Each (group, artifact) pair appears only once in the nodes:
          g.nodes
            .groupBy { case MavenCoordinate(g, a, _) => (g, a) }
            .foreach { case (_, vs) =>
              assert(vs.size == 1)
            }
      }
    }
  }
}

object MavenGraphGen {

  implicit def monadGen: Monad[Gen] = org.scalacheck.GenMonad

  // We want a small universe of groups and artifacts
  val genMavenGroup: Gen[MavenGroup] =
    Gen.oneOf((0 to 10).map(_.toString)).map(MavenGroup(_))

  val genMavenArt: Gen[MavenArtifactId] =
    Gen.oneOf((0 to 10).map(_.toString)).map(MavenArtifactId(_))

  val genVersion: Gen[Version] =
    for {
      major <- Gen.choose(0, 20)
      minor <- Gen.choose(0, 100)
      patch <- Gen.choose(0, 1000)
    } yield Version(s"$major.$minor.$patch")

  val genMavenCoord: Gen[MavenCoordinate] =
    for {
      g <- genMavenGroup
      a <- genMavenArt
      v <- genVersion
    } yield MavenCoordinate(g, a, v)

  def zip[A, B](a: Gen[A], b: Gen[B]): Gen[(A, B)] = a.flatMap { aa =>
    b.map((aa, _))
  }

  // explicitly make a DAG
  def dag[T](g: Gen[T], nodes: Int, maxDeps: Int): Gen[Graph[T, Unit]] = {
    def genSet[A](g: Gen[A], size: Int, s: Set[A]): Gen[Set[A]] =
      if (s.size >= size) Gen.const(s)
      else g.flatMap { a => genSet(g, size, s + a) }

    val gnodes: Gen[Set[T]] = genSet(g, nodes, Set.empty)

    gnodes.flatMap { nodes =>
      Foldable[List].foldM(nodes.toList, Graph.empty[T, Unit]) { (g, n) =>
        // pick a bunch of edges for n to nodes in g
        val existing = g.nodes.toVector
        for {
          edges <- Gen.choose(0, maxDeps)
          ns <- Gen.pick(math.min(edges, g.nodes.size), existing)
          g1 = ns.foldLeft(g) { (g, n1) => g.addEdge(Edge(n, n1, ())) }
        } yield g1
      }
    }
  }

  def decorateRandomly[A, B, C](g: Graph[A, Unit], b: Gen[B])(
      fn: (A, B) => C
  ): Gen[Graph[C, Unit]] =
    Foldable[List].foldM(g.edgeIterator.toList, Graph.empty[C, Unit]) {
      case (g, Edge(src, dst, ())) =>
        for {
          v1 <- b
          v2 <- b
        } yield g.addEdge(Edge(fn(src, v1), fn(dst, v2), ()))
    }

  def genMavenGraphSized(
      size: Int,
      maxDeps: Int
  ): Gen[Graph[MavenCoordinate, Unit]] =
    for {
      unVDag <- dag(zip(genMavenGroup, genMavenArt), size, maxDeps)
      vDag <- decorateRandomly(unVDag, genVersion) { case ((g, a), v) =>
        MavenCoordinate(g, a, v)
      }
    } yield vDag

  val genMavenGraph: Gen[Graph[MavenCoordinate, Unit]] =
    for {
      sz <- Gen.choose(0, 100)
      deps <- Gen.choose(3, 30)
      g <- genMavenGraphSized(sz, deps)
    } yield g
}

