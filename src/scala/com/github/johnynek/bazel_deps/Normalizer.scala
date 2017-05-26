package com.github.johnynek.bazel_deps

import cats.data.{ Validated, ValidatedNel }

object Normalizer {
  type Candidate = Either[Set[Version], Version]
  type Table = Map[Node, List[(Option[MavenCoordinate], Candidate)]]

  type Node = UnversionedCoordinate
  /**
   * assumes every depth less than d has no duplication. Looks at d and greater
   * and updates the dependency graph.
   */
  def apply(graph: Graph[MavenCoordinate, Unit],
    declared: Dependencies,
    opts: Options): Option[Graph[MavenCoordinate, Unit]] = {

    @annotation.tailrec
    def fixTable(table: Table): Table = {

      def errorize(node: Node): Table = {
        val items = table(node)
        val versions = Left(items.foldLeft(Set.empty[Version]) {
          case (set, (_, Left(vs))) => set | vs
          case (set, (_, Right(v))) => set + v
        })
        table.updated(node, items.map { case (p, _) => (p, versions) })
      }

      def isErroneous(node: Option[Node]): Boolean = node match {
        case None =>
          false
        case Some(n) =>
          table(n).exists(_._2.isLeft)
      }

      def isAmbiguous(node: Option[Node]): Boolean = node match {
        case None =>
          false
        case Some(n) =>
          table(n).collect { case (_, Right(v)) => v }.toSet.size > 1
      }

      def disambiguate(node: Node): Table =
        table(node)
          .map(_._1.map(_.unversioned))
          .find(isAmbiguous) match {
            case None => fixVersion(node) // note that parents are not ambiguous
            case Some(None) => sys.error("unreachable, roots are never ambiguous")
            case Some(Some(p)) => disambiguate(p)
          }

      // invariant: all of node's parents must be unambiguous
      def fixVersion(node: Node): Table = {
        val items = table(node)
        val versions = items.map(_._2).toSet
        val dups = versions.collect { case Right(v) => v }
        val rootVersion = dups.iterator.find { v =>
          declared.roots.contains(MavenCoordinate(node, v))
        }
        pickCanonical(node, rootVersion, dups, opts) match {
          case Validated.Valid(m) =>
            val newItems = items.map { case (p, _) => (p, Right(m.version)) }
            table.updated(node, newItems)
            // requirement is that isAmbiguous(node) is now false
          case Validated.Invalid(errorMessages) =>
            errorize(node)
        }
      }

      table.iterator
        .map(_._1)
        .find { n => isAmbiguous(Some(n)) } match {
          case None => table
          case Some(node) => fixTable(disambiguate(node))
        }
    }

    val table: Table =
      graph.nodes.groupBy(_.unversioned)
        .map { case (u, ns) =>
          val values = ns.toList.flatMap { n =>
            // we have lost if a node is explicitly declared
            // for instance, guava is declared and transitive,
            // but at this stage we can't see that reliably.
            // we need to take a list of roots with their full metadata
            val dependsOnN = graph.hasDestination(n).toList
            val v: Candidate = Right(n.version)
            if (dependsOnN.isEmpty) {
              List((None, v))
            }
            else {
              dependsOnN.map { case Edge(s, _, _) =>
                (Some(s), v)
              }
            }
          }
          (u, values)
        }

    val newTable = fixTable(table)
    rewrite(graph, newTable).map { rewrittenGraph =>
      // We now filter only those nodes that are in the
      // reflexive transitive closure of the declared nodes
      val reachable = rewrittenGraph.reflexiveTransitiveClosure(declared.roots.toList)
      // remove all nodes not in this set
      (rewrittenGraph.nodes -- reachable).foldLeft(rewrittenGraph)(_.removeNode(_))
    }
  }

  private def compact(t: Table): Map[UnversionedCoordinate, Version] =
    t.iterator.map { case (k, vs) =>
      vs.headOption match {
        case None => sys.error("broken table")
        case Some((_, Left(_))) => sys.error("erroneous table")
        case Some((_, Right(v0))) => (k, v0)
      }
    }.toMap

  def isKeeper(m: MavenCoordinate, t: Table): Boolean =
    t.get(m.unversioned) match {
      case None => false
      case Some(vs) => vs.forall {
        case (_, Right(v)) => m.version == v
        case (_, Left(_)) => false
      }
    }

  private def rewrite(g: Graph[MavenCoordinate, Unit], t: Table) = {
    if (t.forall { case (_, vs) => vs.forall(_._2.isRight) }) {
      val versions = g.nodes.groupBy(_.unversioned).mapValues(_.map(_.version))
      val canonicals = compact(t)

      def retarget(g0: Graph[MavenCoordinate, Unit], n0: MavenCoordinate): Graph[MavenCoordinate, Unit] =
        if (isKeeper(n0, t)) {
          val dependencies = g0.hasSource(n0).map(_.destination)
          dependencies.foldLeft(g0)(retarget)
        }
        else {
          // rewrite all the edges that terminate at us
          // things that depend on us need to start depending on someone else
          val u = n0.unversioned
          val version = canonicals(u)
          val newTarget = MavenCoordinate(u, version)
          g0.hasDestination(n0).foldLeft(g0) { (g, edge) =>
            g.removeEdge(edge).addEdge(Edge(edge.source, newTarget, ()))
          }
        }

      Some(g.roots.foldLeft(g)(retarget))
    }
    else None
  }

  private def pickCanonical(
    unversioned: UnversionedCoordinate,
    rootVersion: Option[Version],
    duplicates: Set[Version],
    opts: Options): ValidatedNel[String, MavenCoordinate] =
      opts.getVersionConflictPolicy
        .resolve(rootVersion, duplicates)
        .map { v => MavenCoordinate(unversioned, v) }
}
