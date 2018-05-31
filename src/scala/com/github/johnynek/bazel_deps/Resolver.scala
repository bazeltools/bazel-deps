package com.github.johnynek.bazel_deps

import scala.collection.immutable.SortedMap
import scala.util.Try
import scala.util.control.NonFatal
import cats.MonadError
import cats.implicits._

case class ResolveFailure(message: String,
  m: MavenCoordinate,
  extension: String,
  failures: List[Exception]) extends Exception(message)

trait Resolver[F[_]] {
  implicit def resolverMonad: MonadError[F, Throwable]

  def getShas(m: List[MavenCoordinate]): F[SortedMap[MavenCoordinate, ResolvedSha1Value]]

  // Build the entire transitive graph of a set of coordinates
  def buildGraph(coords: List[MavenCoordinate], m: Model): F[Graph[MavenCoordinate, Unit]]

  def run[A](fa: F[A]): Try[A]
}

trait SequentialResolver[F[_]] extends Resolver[F] {
  // This transitively adds the entire reachable graph of dep
  // to the current deps.
  def addToGraph(deps: Graph[MavenCoordinate, Unit], dep: MavenCoordinate, m: Model): F[Graph[MavenCoordinate, Unit]]

  def addAll(deps: Graph[MavenCoordinate, Unit], coords: List[MavenCoordinate], m: Model): F[Graph[MavenCoordinate, Unit]] =
    coords.foldM(deps)(addToGraph(_, _, m))

  def buildGraph(coords: List[MavenCoordinate], m: Model): F[Graph[MavenCoordinate, Unit]] =
    addAll(Graph.empty, coords, m)
}
