package com.github.johnynek.bazel_deps

case class Edge[N, E](source: N, destination: N, label: E)

case class Graph[N, E](nodes: Set[N], edges: Map[N, Set[Edge[N, E]]]) {
  def addNode(n: N): Graph[N, E] = Graph(nodes + n, edges)
  def addEdge(e: Edge[N, E]): Graph[N, E] = {
    val n = addNode(e.source).addNode(e.destination).nodes
    val sE = edges.getOrElse(e.source, Set.empty[Edge[N, E]])
    val e2 = edges + (e.source -> (sE + e))
    val dE = e2.getOrElse(e.destination, Set.empty[Edge[N, E]])
    val e3 = e2 + (e.destination -> (dE + e))
    Graph(n, e3)
  }
  def children(n: N): Set[Edge[N, E]] =
    edges.getOrElse(n, Set.empty).filter(_.source == n)

  def parents(n: N): Set[Edge[N, E]] = ???
  def transitiveChildren(n: N): Set[N] = ???
  def show(implicit ord: Ordering[N] = null): String = {
    def sorted(n: Set[N]): List[N] =
      if (ord == null) n.toList else { n.toList.sorted }

    sorted(nodes).flatMap { s =>
      val es = sorted(children(s).map(_.destination))
      es.map { d =>
        s"$s -> $d"
      }
    }
  }.mkString("\n")
}

object Graph {
  def empty[N, E]: Graph[N, E] = Graph(Set.empty, Map.empty)
}
