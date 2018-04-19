package com.github.johnynek.bazel_deps

import coursier.{Fetch, Cache, Resolution}
import coursier.util.Task
import cats.{Monad, MonadError, Traverse}
import cats.data.{Nested, NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import org.slf4j.LoggerFactory
import scala.collection.immutable.SortedMap
import scala.util.{Failure, Try}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

class CoursierResolver(servers: List[MavenServer], ec: ExecutionContext, runTimeout: Duration) extends Resolver[Task] {
  // TODO: add support for a local file cache other than ivy
  private[this] val repos = Cache.ivy2Local :: servers.map { ms => coursier.MavenRepository(ms.url) }

  private[this] val fetch = Fetch.from(repos, Cache.fetch[Task]())

  private[this] val logger = LoggerFactory.getLogger("bazel_deps.CoursierResolver")

  // TODO: we should be tracking server ids along with the artifacts
  // below, to give more accurate results when multiple servers are
  // used.
  private[this] val serverId: String = servers match {
    case Nil =>
      logger.warn(s"resolver is only using a local ivy cache")
      ""
    case m :: ms =>
      if (ms.nonEmpty) logger.warn(s"server ids may be unreliable, assuming all from $m")
      m.id
  }

  implicit def resolverMonad: MonadError[Task, Throwable] =
    new MonadError[Task, Throwable] {
      def pure[A](a: A) = Task.point(a)
      def flatMap[A, B](fa: Task[A])(fn: A => Task[B]) = fa.flatMap(fn)
      def handleErrorWith[A](fa: Task[A])(rec: Throwable => Task[A]) =
        Task { implicit ec =>
          val m: MonadError[Future, Throwable] =
            cats.instances.future.catsStdInstancesForFuture(ec)
          m.handleErrorWith(fa.future)(t => rec(t).future)
        }
      def raiseError[A](e: Throwable): Task[A] =
        Task(_ => Future.failed(e))
      def tailRecM[A, B](a0: A)(f: A => Task[Either[A, B]]): Task[B] =
        Task { implicit ec =>
          val m: MonadError[Future, Throwable] =
            cats.instances.future.catsStdInstancesForFuture(ec)
          m.tailRecM(a0)(a => f(a).future)
        }
    }

  def run[A](fa: Task[A]): Try[A] = Try(Await.result(fa.value(ec), runTimeout))

  case class FileErrorException(error: coursier.FileError) extends Exception(error.describe)
  case class DownloadFailures(messages: NonEmptyList[String]) extends Exception("resolution errors:\n" + messages.toList.mkString("\n"))

  def getShas(m: List[MavenCoordinate]): Task[SortedMap[MavenCoordinate, ResolvedSha1Value]] = {

    type L[x] = ValidatedNel[String, x]
    type N[x] = Nested[Task, L, x]

    def lookup(c: MavenCoordinate): N[ResolvedSha1Value] = {

      def downloadSha1s(as: List[coursier.Artifact]): Task[Option[ResolvedSha1Value]] =
        as.foldM(Option.empty[ResolvedSha1Value]) {
          case (s @ Some(r), _) => Task.point(s)
          case (None, a) => downloadSha1(a)
        }

      def downloadSha1(a: coursier.Artifact): Task[Option[ResolvedSha1Value]] =
        Cache.file[Task](a).run.map {
          case Left(error) =>
            logger.info(s"failure to download ${a.url}, ${error.describe}")
            None
          case Right(file) =>
            val o = Sha1Value.parseFile(file).map(s => ResolvedSha1Value(s, serverId)).toOption
            o.foreach { r =>
              logger.info(s"SHA-1 for ${c.asString} downloaded from ${a.url} (${r.sha1Value.toHex})")
            }
            o
        }

      def computeSha1(artifact: coursier.Artifact): Task[ResolvedSha1Value] =
        Cache.file[Task](artifact).run.flatMap { e =>
          resolverMonad.fromTry(e match {
            case Left(error) =>
              Failure(FileErrorException(error))
            case Right(file) =>
              Sha1Value.computeShaOf(file).map { sha =>
                logger.info(s"SHA-1 for ${c.asString} computed from ${artifact.url} (${sha.toHex})")
                ResolvedSha1Value(sha, serverId)
              }
          })
        }

      def computeSha1s(as: List[coursier.Artifact]): Task[ResolvedSha1Value] =
        as match {
          case Nil => resolverMonad.raiseError(new RuntimeException(s"we could not download the artifact ${c.asString} to compute the hash"))
          case head :: tail =>
            resolverMonad.handleErrorWith(computeSha1(head))(_ => computeSha1s(tail))
        }

      val module = coursier.Module(c.group.asString, c.artifact.asString, Map.empty)
      val version = c.version.asString
      val f = Cache.fetch[Task]()
      val task = Fetch.find[Task](repos, module, version, f).run

      /*
       * we use Nested here to accumulate all the errors so we can
       * present all to the user, not just one at a time.
       *
       * Note, we could have a custom Task applicative here to make
       * sure we run in parallel, but the default product/map2 will
       * probably be sequential.
       *
       * See cats.Parallel for this.
       */
      Nested[Task, L, ResolvedSha1Value](task.flatMap {
        case Left(errors) =>
          val nel = NonEmptyList.fromList(errors.toList)
            .getOrElse(NonEmptyList("<empty message>", Nil))
          Task.point(Validated.invalid(nel))
        case Right((src, proj)) =>
          val dep = coursier.Dependency(module, version)

          // TODO, this does not seem like the idea thing, but it seems to work.
          val artifacts = src.artifacts(dep, proj, None)
            .iterator
            .filter(_.url.endsWith(".jar"))
            .toList

          if (artifacts.isEmpty) resolverMonad.raiseError(new RuntimeException(s"no artifacts for ${c.asString} found"))
          else {
            downloadSha1s(artifacts.flatMap(_.extra.get("SHA-1"))).flatMap {
              case Some(s) => Task.point(s)
              case None => computeSha1s(artifacts)
            }.map(Validated.valid(_))
          }
      })
    }

    val g: MavenCoordinate => N[(MavenCoordinate, ResolvedSha1Value)] =
      x => lookup(x).map(x -> _)

    Traverse[List].traverse(m)(g).value.flatMap {
      case Validated.Valid(xs) =>
        Task.point(SortedMap(xs: _*))
      case Validated.Invalid(errors) =>
        resolverMonad.raiseError(DownloadFailures(errors))
    }
  }

  // Build the entire transitive graph of a set of coordinates
  def buildGraph(coords: List[MavenCoordinate], m: Model): Task[Graph[MavenCoordinate, Unit]] = {
    def toDep(mc: MavenCoordinate): coursier.Dependency =
      coursier.Dependency(
        coursier.Module(mc.group.asString, mc.artifact.asString),
        mc.version.asString)

    def toCoord(cd: coursier.Dependency): MavenCoordinate =
      MavenCoordinate(
        MavenGroup(cd.module.organization),
        MavenArtifactId(cd.module.name),
        Version(cd.version))

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
