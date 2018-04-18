package com.github.johnynek.bazel_deps

import coursier.{Fetch, Cache, Resolution}
import coursier.util.Task
import cats.Monad
import scala.collection.immutable.SortedMap
import scala.util.Try

class CoursierResolver(servers: List[MavenServer]) extends Resolver[Task] {
  // TODO: add support for a local file cache other than ivy
  val repos = Cache.ivy2Local :: servers.map { ms => coursier.MavenRepository(ms.url) }

  val fetch = Fetch.from(repos, Cache.fetch[Task]())

  implicit def resolverMonad: Monad[Task] = ???

  def run[A](fa: Task[A]): Try[A] = ???
  def getShas(m: List[MavenCoordinate]): Task[SortedMap[MavenCoordinate, Try[ResolvedSha1Value]]] =
    ???

  // Build the entire transitive graph of a set of coordinates
  def buildGraph(coords: List[MavenCoordinate], m: Model): Task[Graph[MavenCoordinate, Unit]] = {
    def toDep(mc: MavenCoordinate): coursier.Dependency = ???

    def toCoord(cd: coursier.Dependency): MavenCoordinate = ???

    val roots: Set[coursier.Dependency] = coords.map(toDep).toSet

    Resolution(roots).process.run(fetch).map { res =>
      val depCache = res.finalDependenciesCache

      depCache.foldLeft(Graph.empty[MavenCoordinate, Unit]) { case (g, (n, deps)) =>
        val cnode = toCoord(n)
        val g1 = g.addNode(cnode)
        deps.foldLeft(g1) { (g, dep) =>
          g.addEdge(Edge(cnode, toCoord(dep), ()))
        }
      }
    }
  }
}
