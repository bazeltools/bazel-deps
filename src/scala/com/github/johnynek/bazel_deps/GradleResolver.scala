package com.github.johnynek.bazel_deps

import cats.MonadError
import cats.data.{NonEmptyList, Validated, ValidatedNec}
import io.circe.jawn.JawnParser
import java.nio.file.{Path, Paths, Files}
import scala.collection.immutable.SortedMap
import scala.collection.mutable.{Map => MMap}
import scala.util.{Failure, Success, Try}

import cats.implicits._

class GradleResolver(
    rootPath: Path,
    versionConflictPolicy: VersionConflictPolicy,
    gradleTpe: ResolverType.Gradle,
    getShasFn: List[MavenCoordinate] => Try[SortedMap[MavenCoordinate, ResolvedShasValue]]
) extends CustomResolver {

  implicit val tryMergeGradleLock = GradleLockDependency.mergeGradleLockDeps(versionConflictPolicy)

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
  ): Try[Map[String, GradleLockDependency]] =
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
      .map(_.getOrElse(Map.empty))
    )

  private def assertConnectedDependencyMap(
      depMap: Map[String, GradleLockDependency]
  ): Try[Unit] =  {
    // Use validated to collect all the missing errors, not just the first
    type V[+A] = ValidatedNec[(String, String), A]

    val unit = Validated.valid(())
    def assertDep(key: String, gld: GradleLockDependency): ValidatedNec[(String, String), Unit] =
      gld.transitive.getOrElse(Nil).traverse_[V, Unit] { luK =>
        if (!depMap.contains(luK)) {
          Validated.invalidNec((luK, key))
        }
        else unit
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
          val errs = nonEmpty.flatMap(_.toList).sortBy(_.swap)
          Failure(
            new Exception("unable to find these referenced transitive deps in dependencies: " +
              errs.iterator.map { case (luK, key) => s"$key -> $luK" }.mkString("[", ", ", "]")
            )
          )
      }
  }

private def cleanUpMap(
  // invariant: depMap is fully connected, all nodes in transitive exist as a key
  depMap: Map[String, GradleLockDependency]
): Map[String, GradleLockDependency] = {
    // for no content deps there is nothing to fetch/no sha to operate on.
    val noContentDeps: Map[String, Option[Version]] = gradleTpe.getNoContentDeps

    def matchNoContentRes(k: String, v: GradleLockDependency): Boolean =
      noContentDeps.get(k) match {
        case None => false
        case Some(None) => true
        case Some(Some(Version(vstr))) => vstr == v.locked.getOrElse("")
      }

    // Remove gradle projects, these are source dependencies
    val gradleProjectsRemoved = depMap.filter(_._2.project != Some(true))

    gradleProjectsRemoved
      .foldLeft(Map.empty[String, GradleLockDependency]) { case (p, (nK, nV)) =>
        @annotation.tailrec
        def go(parents: List[String], loop: Boolean, acc: List[String])
            : List[String] = {
          parents match {
            case h :: t =>
              val hData = gradleProjectsRemoved.getOrElse(
                h,
                // should be impossible due to invariant
                sys.error(s"""
                  |Map in invalid state
                  |tried to get: $h but it wasn't present
                  |this dependency is a project? Not expected here.
                  |Looking for $nK ---> $nV""".stripMargin)
              )

              if (matchNoContentRes(h, hData)) {
                go(
                  t,
                  true,
                  gradleProjectsRemoved
                    .get(h)
                    .flatMap(_.transitive)
                    .getOrElse(Nil) ::: acc
                )
              } else {
                go(t, loop, h :: acc)
              }
            case Nil =>
              // we are recursing a transitive chain, we need to repeat the filter here
              val lst = acc.distinct.filter(gradleProjectsRemoved.contains(_)).sorted

              if (loop) go(lst, false, Nil)
              else lst
          }
        }

        val removeUnused =
          nV.transitive.getOrElse(Nil).filter(gradleProjectsRemoved.contains(_))

        p.updated(nK, nV.copy(transitive = Some(go(removeUnused, false, Nil))))
      }
      .filter { case (k, v) =>
        // We keep these long enough to ensure we can traverse them to make the graph work.
        !matchNoContentRes(k, v)
      }
  }

  private def ignoreEdge(src: MavenCoordinate, dst: MavenCoordinate): Boolean =
    gradleTpe.ignoreDependencyEdge match {
      case Some(missing) =>
        val curEdge: (String, String) = (
          s"${src.group.asString}:${src.artifact.artifactId}",
          s"${dst.group.asString}:${dst.artifact.artifactId}"
        )

        missing(curEdge)
      case None => false
    }

  def buildGraphFromDepMap(
    depMap: Map[String, GradleLockDependency]
  ): Try[Graph[MavenCoordinate, Unit]] =
    assertConnectedDependencyMap(depMap)
      .map(_ => cleanUpMap(depMap))
      .flatMap { depMap =>
        val cache = MMap[String, Try[MavenCoordinate]]()

        def toCoord(k: String): Try[MavenCoordinate] =
          cache.getOrElseUpdate(k,
            depMap.get(k) match {
              case Some(resolvedInfo) =>
                Try {
                  // this parsing could fail, so we do inside a try
                  val e = k.split(':')
                  val org = e(0)
                  val nme = e(1)

                  MavenCoordinate(
                    MavenGroup(org),
                    MavenArtifactId(
                      nme,
                      gradleTpe.getContentTypeOverride.getOrElse(k, "jar"),
                      ""
                    ),
                    Version(resolvedInfo.locked.getOrElse(""))
                  )
                }
              case None =>
                Failure(new Exception(s"Unable to lookup info about $k in dep map"))
            })

        depMap.toList.sortBy(_._1)
        .foldM(Graph.empty[MavenCoordinate, Unit]) { case (g, (n, deps)) =>
          for {
            cnode <- toCoord(n)
            transitive <- deps.transitive.getOrElse(Nil).traverse(toCoord(_))
          } yield {
            val g1 = g.addAllNodes(cnode :: transitive)

            transitive.foldLeft(g1) { case (g, revDep) =>
              if (ignoreEdge(revDep, cnode)) g
              else g.addEdge(Edge(revDep, cnode, ()))
            }
          }
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
        Map[UnversionedCoordinate, Set[Edge[MavenCoordinate, Unit]]]
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
