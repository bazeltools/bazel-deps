package com.github.johnynek.bazel_deps

import cats.MonadError
import cats.data.{NonEmptyList, Validated, ValidatedNec}
import io.circe.jawn.JawnParser
import java.nio.file.{Path, Paths, Files}
import scala.collection.immutable.SortedMap
import scala.collection.mutable.{Map => MMap}
import scala.util.{Failure, Success, Try}
import org.slf4j.LoggerFactory

import cats.implicits._

class GradleResolver(
    rootPath: Path,
    versionConflictPolicy: VersionConflictPolicy,
    gradleTpe: ResolverType.Gradle,
    getShasFn: List[MavenCoordinate] => Try[SortedMap[MavenCoordinate, ResolvedShasValue]]
) extends CustomResolver {

  private val logger = LoggerFactory.getLogger("GradleResolver")

  def resolverType: ResolverType.Gradle = gradleTpe

  implicit val tryMergeGradleLock: TryMerge[GradleLockDependency] =
    GradleLockDependency.mergeGradleLockDeps(versionConflictPolicy)

  private def loadLockFile(
      lockFile: String
  ): Try[GradleLockFile] =
    (Try(new String(Files.readAllBytes(rootPath.resolve(lockFile)), "UTF-8")) match {
      case Success(str) => Success(str)
      case Failure(err) =>
        Failure(new Exception(s"Failed to read ${lockFile}", err))
    })
    .flatMap { content =>
      GradleLockFile.decodeGradleLockFile(new JawnParser, content) match {
        case Right(m) => Success(m)
        case Left(err) =>
          Failure(new Exception(s"Failed to parse ${lockFile}", err))
      }
    }

  // Gradle has compile/runtime/test sepearate classpaths
  // we just want one, so we merge them all
  private def collapseDepTypes(
      lockFile: GradleLockFile
  ): Try[SortedMap[String, GradleLockDependency]] =
    TryMerge.tryMergeAll(
      None,
      NonEmptyList.of(
        lockFile.compileClasspath,
        lockFile.annotationProcessor,
        lockFile.resolutionRules,
        lockFile.runtimeClasspath,
        lockFile.testCompileClasspath,
        lockFile.testRuntimeClasspath
      )
    )
    .map(_.getOrElse(SortedMap.empty[String, GradleLockDependency]))

  private val unit = Validated.valid(())

   /*
    * This depMap represents a graph. The keys are UnversionedCoordinates (and should
    * probably be changed to be those). Inside the GradleLockDependency is a list
    * of nodes that depend on the key (in the field called transitive).
    * 
    * So, this method is making sure that all the values in the union of the transitive
    * lists are also keys of this Map.
    */
  private def assertConnectedDependencyMap(
      depMap: Map[String, GradleLockDependency]
  ): Try[Unit] =  {
    // Use validated to collect all the missing errors, not just the first
    type V[+A] = ValidatedNec[Edge[String, Unit], A]

    def assertDep(key: String, gld: GradleLockDependency): ValidatedNec[Edge[String, Unit], Unit] =
      gld.transitive match {
        case Some(ts) =>
          ts.traverse_[V, Unit] { luK =>
            if (depMap.contains(luK)) unit
            else Validated.invalidNec(Edge(luK, key, ()))
          }
        case None => unit
      }

    // This looks like depMap.toList.traverse_ but we want to avoid
    // making a full copy of a giant graph (toList) when in the common case
    // there are no errors. Instead we collect all the invalid errors
    // and expand them out
    depMap.iterator
      .map { case (k, v) => assertDep(k, v) }
      .collect { case Validated.Invalid(errsU) => errsU }
      .toList match {
        case Nil => Success(())
        case nonEmpty =>
          // sort to make this deterministically ordered in the order they appear
          val errs = nonEmpty.flatMap(_.toList).sortBy(e => (e.source, e.destination))
          Failure(
            new Exception("unable to find these referenced transitive deps in dependencies: " +
              errs.iterator.map { case Edge(src, dest, _) => s"$src -> $dest" }.mkString("[", ", ", "]")
            )
          )
      }
  }

private def cleanUpMap(
  // invariant: depMap is fully connected, all nodes in transitive exist as a key
  depMap: SortedMap[String, GradleLockDependency]
): SortedMap[String, GradleLockDependency] = {

    // for no content deps there is nothing to fetch/no sha to operate on.
    def matchNoContentRes(unversioned: String, v: GradleLockDependency): Boolean =
      gradleTpe.getNoContentDeps.get(unversioned) match {
        case None => false
        case Some(None) => true
        case Some(Some(Version(vstr))) => vstr == v.locked.getOrElse("")
      }

    // Remove gradle projects, these are source dependencies
    val depMapNonProj: SortedMap[String, GradleLockDependency] =
      depMap.filter(_._2.project != Some(true))

    depMapNonProj
      .foldLeft(SortedMap.empty[String, GradleLockDependency]) { case (accGraph, (unversioned, thisGLD)) =>

        /*
         * this method expands a list of transitive deps removing
         * any nonContent jars, and instead adding any edge to the
         * nonContent direction here.
         * 
         * Put another way...
         *      /--c
         * a - b --d
         *      \--e
         * if b is a noContent jar, we rewrite the graph to be:
         *  /--c
         * a --d
         *  \--e
         * 
         * when restart is true and parents.isEmpty we restart with parents = acc that are filtered on depMapNonProj
         * 
         * Invariant: all items in parents and acc are keys of depMapNonProj
         * 
         * this method is only inside this foldLeft to capture the unversioned and thisGLD
         * for debug information if we have violated the invariant somehow and hit
         * sys.error
         */
        @annotation.tailrec
        def relinkNoContents(parents: List[String], restart: Boolean, acc: List[String])
            : List[String] = {
          parents match {
            case h :: t =>
              val hData = depMapNonProj.get(h) match {
                case Some(hGLD) => hGLD
                case None =>
                  // should be impossible due to invariant
                  sys.error(s"""
                    |Map in invalid state
                    |tried to get: $h but it wasn't present
                    |this dependency is a project? Not expected here.
                    |Looking for $unversioned ---> $thisGLD""".stripMargin)
              }

              if (matchNoContentRes(h, hData)) {
                // note: here we are adding hData.transitive that have not been verified
                // to be not "no content jars" themselves.
                // Thus, we have to reset loop to true to check those at the end
                // we always maintain
                // we *do* maintain the invariant that acc1 has keys that are
                // in depMapNonProj
                val nonProjTransitives = hData.transitive match {
                  case Some(ts) => ts.filter(depMapNonProj.contains(_))
                  case None => Nil
                }
                val acc1 = nonProjTransitives ::: acc
                relinkNoContents(t, restart = true, acc1)
              } else {
                // we know that h is not a "no content jar", and that
                // h is in depMapNonProj
                // so we don't have to reset the restart condition
                val acc1 = h :: acc
                relinkNoContents(t, restart, acc1)
              }
            case Nil =>
              // we are recursing a transitive chain
              val lst = acc.distinct

              // recall: all items in acc are in depMapNonProj
              // so we don't need to filter here
              if (restart) relinkNoContents(lst, restart = false, Nil)
              else {
                // this is the only return of this method
                // so we sort once here
                lst.sorted
              }
          }
        }

        // we set up the initial transitives that are in depMapNonProj
        // we have to maintain the invariant that all inputs to relinkNoContents
        val initTrans = thisGLD
          .transitive
          .getOrElse(Nil)
          .filter(depMapNonProj.contains(_))
          
        val newTransitives = relinkNoContents(initTrans, false, Nil)

        accGraph.updated(
          unversioned,
          thisGLD.copy(transitive = Some(newTransitives)))
      }
      .filter { case (k, v) =>
        // We keep these long enough to ensure we can traverse them to make the graph work.
        !matchNoContentRes(k, v)
      }
  }

  private val ignored: Graph[UnversionedCoordinate, Unit] = {
    val g = Graph.empty[UnversionedCoordinate, Unit]
    gradleTpe.ignoreDependencyEdge match {
      case Some(missing) =>
        logger.info(s"ignoreDependencyEdge has ${missing.size} items")
        missing.iterator.flatMap { case (s, d) =>
          UnversionedCoordinate.parse(s)
            .product(UnversionedCoordinate.parse(d)) match {
              case Validated.Valid((suv, duv)) =>
                logger.info(s"ignoring $s -> $d")
                Edge(suv, duv, ()) :: Nil
              case Validated.Invalid(errs) =>
                logger.warn(s"could not parse ignoreDependencyEdge: $s -> $d: $errs")
                Nil
            }
        }
        .foldLeft(g) { (g, e) => g.addEdge(e) }
      case None =>
        logger.info(s"ignoreDependencyEdge is empty.")
        g
    }
  }

  // most groups have no ignored edges, put this here to reduce
  // costs
  private val maybeIgnored: Set[MavenGroup] =
    ignored.edgeIterator.map(_.source.group).toSet

  def ignoreEdge(src: MavenCoordinate, dst: MavenCoordinate): Boolean =
    maybeIgnored(src.group) && {
      ignored.hasEdge(Edge(src.unversioned, dst.unversioned, ()))
    }

  def buildGraphFromDepMap(
    depMap: SortedMap[String, GradleLockDependency]
  ): Try[Graph[MavenCoordinate, Unit]] =
    assertConnectedDependencyMap(depMap)
      .map(_ => cleanUpMap(depMap))
      .flatMap { (depMap: SortedMap[String, GradleLockDependency]) =>
        type V[+A] = ValidatedNec[(String, String), A]
        val cache = MMap[String, V[MavenCoordinate]]()

        def toCoord(k: String): V[MavenCoordinate] =
          cache.getOrElseUpdate(k,
            depMap.get(k) match {
              case Some(resolvedInfo) =>
                val e = k.split(':')
                if (e.length >= 2) {
                  val org = e(0)
                  val nme = e(1)

                  Validated.valid(MavenCoordinate(
                    MavenGroup(org),
                    MavenArtifactId(
                      nme,
                      gradleTpe.getContentTypeOverride.getOrElse(k, "jar"),
                      ""
                    ),
                    Version(resolvedInfo.locked.getOrElse(""))
                  ))
                }
                else {
                  Validated.invalidNec((k, s"could not parse key $k into maven artifact"))
                }
              case None =>
                Validated.invalidNec((k, s"Unable to lookup info about $k in dep map"))
            })

        depMap
          .toList
          .traverse[V, (MavenCoordinate, List[MavenCoordinate])] { case (n, deps) =>
            toCoord(n)
              .product(
                deps.transitive.getOrElse(Nil)
                  .traverse[V, MavenCoordinate](toCoord(_))
              )
          } match {
          case Validated.Valid(edgeList) =>
            val empty = Graph.empty[MavenCoordinate, Unit]
            val withEdges = edgeList.foldLeft(empty) { case (g, (cnode, transitive)) =>
              transitive.foldLeft(g.addNode(cnode)) { case (g, revDep) =>
                // addEdge adds both nodes in the edge
                // but if we ignore the edge, we defensively make sure we add revDep
                // since this graph is validated, it should be added when it appears
                // as a key however
                if (ignoreEdge(revDep, cnode)) {
                  logger.debug(s"ignored: $revDep -> $cnode")
                  g.addNode(revDep)
                }
                else g.addEdge(Edge(revDep, cnode, ()))
              }
            }
            Success(withEdges)
          case Validated.Invalid(errs) =>
            Failure(new Exception(errs.map(_._2)
              .mkString_("failed to find ${errs.length} nodes:\n\t", "\n\t", "")))
        }
      }

  // Build the entire transitive graph of a set of coordinates
  private def buildGraph(
      m: Model
  ): Try[Graph[MavenCoordinate, Unit]] =
    for {
      lfs <- gradleTpe.getLockFiles.traverse(loadLockFile)
      lockFile <- TryMerge.tryMergeAllOr(None, lfs, Success(GradleLockFile.empty))
      depMap <- collapseDepTypes(lockFile)
      graph <- buildGraphFromDepMap(depMap)
    } yield {
      // TODO: this is a little weird that we are only adding the roots
      // but *maybe* this "right" in this mode because we are just adding
      // individual missing items.
      graph.addAllNodes(m.dependencies.roots)
    }

  def resolve(model: Model): Try[
    (
        Graph[MavenCoordinate, Unit],
        SortedMap[MavenCoordinate, ResolvedShasValue],
        Map[UnversionedCoordinate, Set[Edge[MavenCoordinate, Boolean]]]
    )
  ] = {
    def replaced(m: MavenCoordinate): Boolean =
      model.getReplacements.get(m.unversioned).isDefined

    for {
      graph <- buildGraph(model)
      shas <- getShasFn(graph.nodes.filterNot(replaced).toList.sorted)
    } yield (graph, shas, Map.empty)
  }
}
