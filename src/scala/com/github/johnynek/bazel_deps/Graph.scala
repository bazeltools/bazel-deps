package com.github.johnynek.bazel_deps

import org.typelevel.paiges.Doc

case class Edge[N, E](source: N, destination: N, label: E)

case class Graph[N, E](nodes: Set[N], edges: Map[N, Set[Edge[N, E]]]) {
  def toDoc(showN: N => String, showE: E => String): Doc = {
    val tab = nodes
      .toList
      .sortBy(showN)
      .map { n =>
        val strN = showN(n)
        val es = hasSource(n)
        val edoc = Doc.intercalate(Doc.line, es.map { case Edge(_, d, e) =>
          Doc.text(s"-(${showE(e)})->") + Doc.space + Doc.text(showN(d))
        })
        (strN, edoc)
      }
    Doc.tabulate(' ', ":", tab)
  }

  def edgeIterator: Iterator[Edge[N, E]] =
    edges.iterator.flatMap { case (n, es) =>
      // each edge appears twice, only return the ones where
      // source is the key
      es.iterator.filter(_.source == n)
    }

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
    val newG =
      edges.getOrElse(n, Set.empty)
        .foldLeft(this)(_.removeEdge(_))
    Graph(newG.nodes - n, newG.edges)
  }
  def removeEdge(e: Edge[N, E]): Graph[N, E] = {
    def go(em: Map[N, Set[Edge[N, E]]], n: N) = em.get(n) match {
      case Some(es) =>
        val e1 = es - e
        if (e1.isEmpty) em - (n)
        else em + (n -> e1)
      case None => em
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

  /**
   * the result contains the input
   */
  def reflexiveTransitiveClosure(n: List[N]): Set[N] = {
    @annotation.tailrec
    def loop(stack: List[N], acc: Set[N]): Set[N] = stack match {
      case Nil => acc
      case head::tail =>
        val nodes = hasSource(head)
          .iterator
          .map(_.destination)
          .filterNot(acc)
          .toList
        val newStack = nodes ::: tail
        val newAcc = acc ++ nodes
        loop(newStack, newAcc)
    }
    loop(n, n.toSet)
  }
}

object Graph {
  def empty[N, E]: Graph[N, E] = Graph(Set.empty, Map.empty)
}
