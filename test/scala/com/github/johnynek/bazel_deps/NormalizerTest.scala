package com.github.johnynek.bazel_deps

import org.scalatest.funsuite.AnyFunSuite

class NormalizerTest extends AnyFunSuite {
  test("test normalization with a simple example") {

    val g = Graph.empty[MavenCoordinate, Unit]

    implicit class AbuseGraph(val g: Graph[MavenCoordinate, Unit]) {
      def add(from: String, to: String): Graph[MavenCoordinate, Unit] =
        g.addEdge(Edge(MavenCoordinate(from), MavenCoordinate(to), ()))
    }

    val cat1 = "a:cat:1.0"
    val snake1 = "a:snake:1.0"
    val bird1 = "a:bird:1.0"
    val bird2 = "a:bird:2.0"
    val seed1 = "a:seed:1.0"
    val dog1 = "a:dog:1.0"

    /** a:cat:1.0 -> a:bird:1.0 a:snake:1.0 -> a:bird:2.0 a:bird:1.0 ->
      * a:worm:1.0 a:bird:2.0 -> a:seed:1.0
      *
      * roots: cat, snake
      *
      * goal: no bird:1.0 or worm:1.0
      */
    val finalG = g
      .add(cat1, bird1)
      .add(snake1, bird2)
      .add(bird1, "a:worm:1.0")
      .add(bird2, seed1)
      .addNode(MavenCoordinate(dog1))

    Normalizer(
      finalG,
      Set(cat1, snake1, dog1).map(MavenCoordinate(_)),
      VersionConflictPolicy.default
    ) match {
      case Some(normalG) =>
        val expected = g
          .add(cat1, bird2)
          .add(snake1, bird2)
          .add(bird2, seed1)
          .addNode(MavenCoordinate(dog1))

        assert(normalG === expected)

      case None => fail("could not normalize")
    }
  }

  test("test normalization with goofy circular transitive dep example") {

    val g = Graph.empty[MavenCoordinate, Unit]

    implicit class AbuseGraph(val g: Graph[MavenCoordinate, Unit]) {
      def add(from: String, to: String): Graph[MavenCoordinate, Unit] =
        g.addEdge(Edge(MavenCoordinate(from), MavenCoordinate(to), ()))
    }

    val cat1_2 = "a:cat:1.2"
    val mouse1_2 = "a:mouse:1.2"
    val cat1_1 = "a:cat:1.1"
    val mouse1_1 = "a:mouse:1.1"

    /** a:cat:1.2 -> a:mouse:1.2 a:mouse:1.2 -> a:cat:1.1 a:cat:1.1 ->
      * a:mouse:1.1
      *
      * roots: cat1_2, mouse1_2
      *
      * goal: don't infinite loop in Normalizer.disambiguate
      */
    val finalG = g
      .add(cat1_2, mouse1_2)
      .add(mouse1_2, cat1_1)
      .add(cat1_1, mouse1_1)

    Normalizer(
      finalG,
      Set(cat1_2, mouse1_2).map(MavenCoordinate(_)),
      VersionConflictPolicy.default
    )
  }
}
