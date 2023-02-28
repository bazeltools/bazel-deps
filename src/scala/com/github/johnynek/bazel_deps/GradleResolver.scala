package com.github.johnynek.bazel_deps

import cats.MonadError
import cats.data.{Validated, ValidatedNel}
import coursier.util.Task
import io.circe.jawn.JawnParser
import cats.implicits._

import java.io.File
import java.nio.file.Path
import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

import cats.implicits._

class GradleResolver(
    servers: List[DependencyServer],
    versionConflictPolicy: VersionConflictPolicy,
    ec: ExecutionContext,
    runTimeout: Duration,
    gradleTpe: ResolverType.Gradle,
    cachePath: Path
) extends Resolver[Task] {

  implicit val tryMergeGradleLock = GradleLockDependency.mergeGradleLockDeps(versionConflictPolicy)

  private[this] lazy val coursierResolver =
    new CoursierResolver(servers, ec, runTimeout, cachePath)

  implicit def resolverMonad: MonadError[Task, Throwable] =
    coursierResolver.resolverMonad

  def getShas(
      m: List[MavenCoordinate]
  ): Task[SortedMap[MavenCoordinate, ResolvedShasValue]] =
    coursierResolver.getShas(m)

  private def loadLockFile(
      lockFile: String
  ): Try[GradleLockFile] =
    (Model.readFile(new File(lockFile)) match {
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

  private def mergeLockFiles(
      lockFiles: List[GradleLockFile]
  ): Try[GradleLockFile] =
    lockFiles.foldM(GradleLockFile.empty) {
      TryMerge.tryMerge(None, _, _)
    }

  // Gradle has compile/runtime/test sepearate classpaths
  // we just want one, so we merge them all
  private def collapseDepTypes(
      lockFile: GradleLockFile
  ): Try[Map[String, GradleLockDependency]] =
    List(
      lockFile.compileClasspath,
      lockFile.annotationProcessor,
      lockFile.resolutionRules,
      lockFile.runtimeClasspath,
      lockFile.testCompileClasspath,
      lockFile.testRuntimeClasspath
    )
    .map(_.getOrElse(Map.empty))
    .foldM(Map.empty[String, GradleLockDependency]) {
      TryMerge.tryMerge(None, _, _)
    }

  private def assertConnectedDependencyMap(
      depMap: Map[String, GradleLockDependency]
  ): Try[Unit] =  {
    // Use validated to collect all the missing errors, not just the first
    type V[+A] = ValidatedNel[(String, String), A]

    def assertDep(key: String, gld: GradleLockDependency): ValidatedNel[(String, String), Unit] =
      gld.transitive.getOrElse(Nil).traverse_[V, Unit] { luK =>
        if (!depMap.contains(luK)) {
          Validated.invalidNel((luK, key))
        }
        else {
          Validated.Valid(())
        }
      }

    // sort to make this deterministically ordered
    depMap.toList.sortBy(_._1).traverse_[V, Unit] { case (k, v) => assertDep(k, v) } match {
      case Validated.Valid(()) => Success(())
      case Validated.Invalid(errs) =>
        Failure(new Exception("unable to find these referenced transitive deps in dependencies: " +
          errs.toList.iterator.map { case (luK, key) => s"$key -> $luK" }.mkString("[", ", ", "]")
        ))
    }
}

private def cleanUpMap(
      depMap: Map[String, GradleLockDependency]
  ): Map[String, GradleLockDependency] = {
    // for no content deps there is nothing to fetch/no sha to operate on.
    val noContentDeps: Map[String, Option[Version]] = gradleTpe.getNoContentDeps
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
                  sys.error(s"""
                    |Map in invalid state
                    |tried to get: $h but it wasn't present
                    |this dependency is a project? Not expected here.
                    |Looking for $nK ---> $nV""".stripMargin)
                )
                val matchNoContentRes = noContentDeps
                  .get(h)
                  .map { innerOpt =>
                    innerOpt == None || innerOpt == Some(
                      Version(hData.locked.getOrElse(""))
                    )
                  }
                  .getOrElse(false)
                if (matchNoContentRes) {
                  go(
                    t,
                    true,
                    gradleProjectsRemoved
                      .get(h)
                      .flatMap(_.transitive)
                      .getOrElse(Nil) ++ acc
                  )
                } else {
                  go(t, loop, h :: acc)
                }
              case Nil =>
                // we are recursing a transitive chain, we need to repeat the filter here
                val lst = acc.sorted.distinct.filter(gradleProjectsRemoved.contains(_))
                if (loop) {
                  go(lst, false, Nil)
                } else {
                  lst
                }

            }
          }

          val removeUnused =
            nV.transitive.getOrElse(Nil).filter(gradleProjectsRemoved.contains(_))

          p + ((nK, nV.copy(transitive = Some(go(removeUnused, false, Nil)))))
        }
        .filter { case (k, v) =>
          // We keep these long enough to ensure we can traverse them to make the graph work.
          val matchNoContentRes = noContentDeps
            .get(k)
            .map { innerOpt =>
              innerOpt == None || innerOpt == Some(
                Version(v.locked.getOrElse(""))
              )
            }
            .getOrElse(false)

          !matchNoContentRes
        }
  }

  //
  private def buildGraphFromDepMap(
    m: Model,
    depMap: Map[String, GradleLockDependency]
  ): Try[Graph[MavenCoordinate, Unit]] = {
    assertConnectedDependencyMap(depMap).map(_ => cleanUpMap(depMap)).flatMap { depMap =>
      def toCoord(k: String): Try[MavenCoordinate] =
        depMap.get(k) match {
          case Some(resolvedInfo) =>
            val e = k.split(':')
            val (org, nme) = (e(0), e(1))

            Success(
              MavenCoordinate(
                MavenGroup(org),
                MavenArtifactId(
                  nme,
                  gradleTpe.getContentTypeOverride.getOrElse(k, "jar"),
                  ""
                ),
                Version(resolvedInfo.locked.getOrElse(""))
              )
            )
          case None =>
            Failure(new Exception(s"Unable to lookup info about $k in dep map"))
        }

      val gradleDependencyGraphTry = depMap.toList.foldM(Graph.empty[MavenCoordinate, Unit]) {
        case (g, (n, deps)) =>
          for {
            cnode <- toCoord(n)
            transitive <- cats
              .Traverse[List]
              .sequence(deps.transitive.getOrElse(Nil).map(toCoord(_)))
          } yield {
            val g1 = g.addAllNodes(cnode :: transitive)
            transitive.foldLeft(g1) { case (g, revDep) =>
              val curEdge = (
                s"${revDep.group.asString}:${revDep.artifact.artifactId}",
                s"${cnode.group.asString}:${cnode.artifact.artifactId}"
              )
              if (
                gradleTpe.ignoreDependencyEdge
                  .map(_.contains(curEdge))
                  .getOrElse(false)
              ) {
                g
              } else {
                g.addEdge(Edge(revDep, cnode, ()))
              }
            }
          }
      }

      gradleDependencyGraphTry.map { graph =>
        graph.addAllNodes(m.dependencies.roots)
      }
    }
  }

  def toTask[A](t: Try[A]): Task[A] =
    t match {
      case Success(value)     => Task.point(value)
      case Failure(exception) => Task.fail(exception)
    }

  // Build the entire transitive graph of a set of coordinates
  def buildGraph(
      coords: List[MavenCoordinate],
      m: Model
  ): Task[Graph[MavenCoordinate, Unit]] =
    toTask(
      for {
        lfs <- gradleTpe.getLockFiles.traverse(loadLockFile)
        lockFile <- mergeLockFiles(lfs)
        depMap <- collapseDepTypes(lockFile)
        graph <- buildGraphFromDepMap(m, depMap)
      } yield graph
    )

  def resolve(model: Model): Task[
    (
        Graph[MavenCoordinate, Unit],
        SortedMap[MavenCoordinate, ResolvedShasValue],
        Map[UnversionedCoordinate, Set[Edge[MavenCoordinate, Unit]]]
    )
  ] = {
    def replaced(m: MavenCoordinate): Boolean =
      model.getReplacements.get(m.unversioned).isDefined

    for {
      graph <- buildGraph(Nil, model)
      shas <- getShas(graph.nodes.filterNot(replaced).toList.sorted)
    } yield (graph, shas, Map.empty)
  }

  def run[A](fa: Task[A]): Try[A] =
    coursierResolver.run(fa)
}
