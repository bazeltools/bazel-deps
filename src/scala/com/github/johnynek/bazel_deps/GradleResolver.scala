package com.github.johnynek.bazel_deps

import cats.MonadError
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
    ec: ExecutionContext,
    runTimeout: Duration,
    gradleTpe: ResolverType.Gradle,
    cachePath: Path
) extends Resolver[Task] {
  private[this] lazy val coursierResolver =
    new CoursierResolver(servers, ec, runTimeout, cachePath)

  implicit def resolverMonad: MonadError[Task, Throwable] =
    coursierResolver.resolverMonad

  def getShas(
      m: List[MavenCoordinate]
  ): Task[SortedMap[MavenCoordinate, ResolvedShasValue]] =
    coursierResolver.getShas(m)

  private def loadLockFiles(
      lockFiles: List[String]
  ): Try[List[GradleLockFile]] =
    lockFiles.traverse { next =>
      (Model.readFile(new File(next)) match {
        case Success(str) => Success(str)
        case Failure(err) =>
          Failure(new Exception(s"Failed to read ${next}", err))
      })
      .flatMap { content =>
        Decoders.decodeGradleLockFile(new JawnParser, content) match {
          case Right(m) => Success(m)
          case Left(err) =>
            Failure(new Exception(s"Failed to parse ${next}", err))
        }
      }
    }

  private def mergeLockFiles(
      lockFiles: List[GradleLockFile]
  ): Try[GradleLockFile] =
    lockFiles.foldM(GradleLockFile.empty) { (s, n) =>
      TryMerge.tryMerge(None, s, n)
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
      .map(_.getOrElse(Map()))
      .foldLeft(Try(Map[String, GradleLockDependency]())) { case (p, n) =>
        p.flatMap { s => TryMerge.tryMerge(None, s, n) }
      }

  private def assertConnectedDependencyMap(
      depMap: Map[String, GradleLockDependency]
  ): Try[Unit] =  {
    val sunit = Success(())

    def assertDep(key: String, gld: GradleLockDependency): Try[Unit] =
      gld.transitive.getOrElse(Nil).traverse_ { luK =>
        if (!depMap.contains(luK)) {
          Failure(new Exception(s"Unable to find $luK, referenced as a transitive dep for $key in dependencies."))
        }
        else {
          sunit
        }
      }

    depMap.toList.traverse_ { case (k, v) => assertDep(k, v) }
  }

  private def cleanUpMap(
      depMap: Map[String, GradleLockDependency]
  ): Map[String, GradleLockDependency] = {
    // for no content deps there is nothing to fetch/no sha to operate on.
    val noContentDeps: Map[String, Option[Version]] = gradleTpe.getNoContentDeps
      // Remove gradle projects, these are source dependencies
      val gradleProjectsRemoved = depMap.filter(_._2.project != Some(true))

      gradleProjectsRemoved
        .foldLeft(Map[String, GradleLockDependency]()) { case (p, (nK, nV)) =>
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
        depMap
          .get(k)
          .map { resolvedInfo =>
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
          }
          .getOrElse(
            Failure(new Exception(s"Unable to lookup info about $k in dep map"))
          )

      val gradleDependencyGraphTry = depMap.foldLeft(Try(Graph.empty[MavenCoordinate, Unit])) {
        case (tryG, (n, deps)) =>
          for {
            g <- tryG
            cnode <- toCoord(n)
            transitive <- cats
              .Traverse[List]
              .sequence(deps.transitive.getOrElse(Nil).map(toCoord(_)))
          } yield {
            val g1 = (cnode :: transitive).foldLeft(g) { case (p, n) =>
              p.addNode(n)
            }
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
        m.dependencies.roots.foldLeft(graph) { case (g, n) =>
          g.addNode(n)
        }
      }
    }
  }

  // Build the entire transitive graph of a set of coordinates
  def buildGraph(
      coords: List[MavenCoordinate],
      m: Model
  ): Task[Graph[MavenCoordinate, Unit]] = {
    loadLockFiles(gradleTpe.getLockFiles)
      .flatMap(mergeLockFiles(_))
      .flatMap(collapseDepTypes(_))
      .flatMap(buildGraphFromDepMap(m, _)) match {
      case Success(value)     => Task.point(value)
      case Failure(exception) => Task.fail(exception)
    }
  }

  def resolve(model: Model): Task[
    (
        Graph[MavenCoordinate, Unit],
        SortedMap[MavenCoordinate, ResolvedShasValue],
        Map[UnversionedCoordinate, Set[Edge[MavenCoordinate, Unit]]]
    )
  ] = {
    buildGraph(Nil, model)
      .flatMap { graph =>
        def replaced(m: MavenCoordinate): Boolean =
          model.getReplacements.get(m.unversioned).isDefined
        for {
          shas <- getShas(graph.nodes.filterNot(replaced).toList.sorted)
        } yield (graph, shas, Map())
      }
  }

  def run[A](fa: Task[A]): Try[A] = {
    coursierResolver.run(fa)
  }

}
