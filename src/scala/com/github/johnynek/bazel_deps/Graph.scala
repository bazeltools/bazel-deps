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
  def hasSource(n: N): Set[Edge[N, E]] =
    edges.getOrElse(n, Set.empty).filter(_.source == n)

  def hasDestination(n: N): Set[Edge[N, E]] =
    edges.getOrElse(n, Set.empty).filter(_.destination == n)

  // These have no parents
  lazy val roots: Set[N] = nodes.filter(hasDestination(_).isEmpty)

  def removeNode(n: N): Graph[N, E] = {
    val badEdges = edges.getOrElse(n, Set.empty)
    val neighs = badEdges.map { e =>
        if (e.destination == n) e.source else e.destination
      }
    val newEs = neighs.foldLeft(edges) { (es, n1) =>
      val newEdges = es(n1).filterNot {
        case Edge(s, d, _) => s == n || d == n
      }
      es + (n1 -> newEdges)
    }
    Graph(nodes - n, newEs)
  }
  def removeEdge(e: Edge[N, E]): Graph[N, E] = {
    def go(em: Map[N, Set[Edge[N, E]]], n: N) = {
      val e1 = em.getOrElse(n, Set.empty) - e
      if (e1.isEmpty) em - (n)
      else em + (n -> e1)
    }
    Graph(nodes, go(go(edges, e.source), e.destination))
  }

  def show(shf: N => String)(implicit ord: Ordering[N] = null): String = {
    def sorted(n: Set[N]): List[N] =
      if (ord == null) n.toList else { n.toList.sorted }

    sorted(nodes).flatMap { s =>
      val es = sorted(hasSource(s).map(_.destination))
      es.map { d =>
        s"${shf(s)} -> ${shf(d)}"
      }
    }
  }.mkString("\n")
}

object Graph {
  def empty[N, E]: Graph[N, E] = Graph(Set.empty, Map.empty)
}
