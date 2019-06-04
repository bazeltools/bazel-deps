package com.github.johnynek.bazel_deps

import cats.data.{ Validated, ValidatedNel }
import org.typelevel.paiges.Doc
import org.slf4j.LoggerFactory

object Normalizer {
  type Candidate = Either[Set[Version], Version]
  type Node = UnversionedCoordinate
  type Table = Map[Node, List[(Option[MavenCoordinate], Candidate)]]

  private[this] val logger = LoggerFactory.getLogger("Normalizer")

  def tableToDoc(t: Table): Doc = {
    val pairs = t.map { case (n, versions) =>
      val versionDocs = versions.map { case (opt, c) =>
        val mvn = opt.fold("None")(_.asString)
        val cand = Doc.str(c) // could be prettier
        (mvn, cand)
      }
      val tabbed = Doc.tabulate(' ', " => ", versionDocs)
      (n.asString, tabbed)
    }
    Doc.tabulate(' ', " -> ", pairs)
  }

  /**
   * assumes every depth less than d has no duplication. Looks at d and greater
   * and updates the dependency graph.
   */
  def apply(graph: Graph[MavenCoordinate, Unit],
    roots: Set[MavenCoordinate],
    vcf: VersionConflictPolicy): Option[Graph[MavenCoordinate, Unit]] = {

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

      def disambiguate(node: Node): Table = disambiguateHelper(node, Set())

      def disambiguateHelper(node: Node, visited: Set[Node]): Table = {
        table(node)
          .map(_._1.map(_.unversioned))
          .find(isAmbiguous) match {
            case None => fixVersion(node) // note that parents are not ambiguous
            case Some(None) => sys.error("unreachable, roots are never ambiguous")
            case Some(Some(p)) => {
              if (!visited.contains(p)) {
                disambiguateHelper(p, visited + p)
              } else {
                // We found a cycle in the maven dependency graph. Maven is OK with this (why!?),
                // but bazel won't be. However, this might be a cycle in the transitive dependency
                // graph that won't be present in the BUILD files, so we'll allow it for now.
                fixVersion(node)
              }
            }
          }
      }

      // invariant: all of node's parents must be unambiguous
      def fixVersion(node: Node): Table = {
        val items = table(node)
        val versions = items.map(_._2).toSet
        val dups = versions.collect { case Right(v) => v }
        val rootVersion = dups.iterator.find { v =>
          roots.contains(MavenCoordinate(node, v))
        }
        if (rootVersion.isEmpty) {
          logger.error(s"Please, resolve versions conflicts for ${node.asString}")
          items
            .filter(_._1.isDefined)
            .map(x => s"   -> [${x._1.get.asString}] asks for ${x._2.right.toSeq.mkString(", ")}")
            .foreach(logger.error(_))
        }

        pickCanonical(node, rootVersion, dups, vcf) match {
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
    rewrite(graph, roots.toList, newTable)
  }

  private def compact(t: Table): Map[UnversionedCoordinate, Version] =
    t.iterator.map { case (k, vs) =>
      vs.headOption match {
        case None => sys.error("broken table")
        case Some((_, Left(_))) => sys.error("erroneous table")
        case Some((_, Right(v0))) => (k, v0)
      }
    }.toMap

  private def isKeeper(m: MavenCoordinate, t: Table): Boolean =
    t.get(m.unversioned) match {
      case None => false
      case Some(vs) => vs.forall {
        case (_, Right(v)) => m.version == v
        case (_, Left(_)) => false
      }
    }

  private def rewrite(g: Graph[MavenCoordinate, Unit], roots: List[MavenCoordinate], t: Table): Option[Graph[MavenCoordinate, Unit]] = {
    if (t.forall { case (_, vs) => vs.forall(_._2.isRight) }) {
      val canonicals = compact(t)

      @annotation.tailrec
      def addReachable(acc: Graph[MavenCoordinate, Unit], toProcess: List[UnversionedCoordinate], processed: Set[UnversionedCoordinate]): Graph[MavenCoordinate, Unit] =
        toProcess match {
          case Nil => acc
          case h :: tail if processed(h) => addReachable(acc, tail, processed)
          case h :: tail =>
            val versionedH = MavenCoordinate(h, canonicals(h))
            // we want to add this, and all the nodes this versionedH pointed to in original graph
            val dependenciesUv: Set[UnversionedCoordinate] =
              g.hasSource(versionedH).map(_.destination.unversioned)

            val dependencies = dependenciesUv.map { uv =>
              MavenCoordinate(uv, canonicals(uv))
            }
            // add the edges from versionedH -> deps, and versionedH itself (as it may have no deps)
            val newGraph = dependencies.foldLeft(acc) { (g, dep) =>
              g.addEdge(Edge(versionedH, dep, ()))
            }.addNode(versionedH)
            // now process all dependencies:
            addReachable(newGraph, (dependenciesUv.filterNot(processed)).toList ::: toProcess, processed + h)
        }

      val queue = roots.map(_.unversioned)
      Some(addReachable(Graph.empty, queue, Set.empty))
    }
    else None
  }

  private def pickCanonical(
    unversioned: UnversionedCoordinate,
    rootVersion: Option[Version],
    duplicates: Set[Version],
    vcf: VersionConflictPolicy): ValidatedNel[String, MavenCoordinate] =
      vcf
        .resolve(rootVersion, duplicates)
        .map { v => MavenCoordinate(unversioned, v) }
}
