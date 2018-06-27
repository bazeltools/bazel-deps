package com.github.johnynek.bazel_deps

import coursier.{Fetch, Cache, CachePolicy, Resolution, Artifact, Dependency, Project}
import coursier.util.{Task, Schedulable}
import cats.{Monad, MonadError, Traverse}
import cats.data.{Nested, NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import org.slf4j.LoggerFactory
import scala.collection.immutable.SortedMap
import scala.util.{Failure, Try}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

object CoursierResolver {
  // 48 concurrent downloads
  // most downloads are tiny sha downloads so try keep things alive
  lazy val downloadPool = Schedulable.fixedThreadPool(48)
}
class CoursierResolver(servers: List[MavenServer], ec: ExecutionContext, runTimeout: Duration) extends Resolver[Task] {
  // TODO: add support for a local file cache other than ivy
  private[this] val repos = Cache.ivy2Local :: servers.map { ms => coursier.MavenRepository(ms.url) }

  private[this] val fetch = Fetch.from(repos, Cache.fetch[Task](cachePolicy = CachePolicy.FetchMissing, pool = CoursierResolver.downloadPool))

  private[this] val logger = LoggerFactory.getLogger("bazel_deps.CoursierResolver")

  // Instructs the coursier resolver to keep `runtime`-scoped dependencies.
  private[this] val DefaultConfiguration = "default(compile)"

  def serverFor(a: coursier.Artifact): Option[MavenServer] =
    if (a.url.isEmpty) None
    else servers.find { ms => a.url.startsWith(ms.url) }

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

  def getShas(m: List[MavenCoordinate]): Task[SortedMap[MavenCoordinate, ResolvedShasValue]] = {

    type L[x] = ValidatedNel[String, x]
    type N[x] = Nested[Task, L, x]

    def lookup(c: MavenCoordinate): N[ResolvedShasValue] = {

      def downloadShas(digestType: DigestType, as: List[coursier.Artifact]): Task[Option[ShaValue]] =
        as.foldM(Option.empty[ShaValue]) {
          case (s @ Some(r), _) => Task.point(s)
          case (None, a) => downloadSha(digestType, a)
        }

      def downloadSha(digestType: DigestType, a: coursier.Artifact): Task[Option[ShaValue]] =
        Cache.file[Task](a, cachePolicy = CachePolicy.FetchMissing, pool = CoursierResolver.downloadPool).run.map {
          case Left(error) =>
            logger.info(s"failure to download ${a.url}, ${error.describe}")
            None
          case Right(file) =>
            val o = ShaValue.parseFile(digestType, file).toOption
            o.foreach { r =>
              logger.info(s"SHA-1 for ${c.asString} downloaded from ${a.url} (${r.toHex})")
            }
            o
        }

      def computeSha(digestType: DigestType, artifact: coursier.Artifact): Task[ShaValue] =
        Cache.file[Task](artifact, cachePolicy = CachePolicy.FetchMissing, pool = CoursierResolver.downloadPool).run.flatMap { e =>
          resolverMonad.fromTry(e match {
            case Left(error) =>
              Failure(FileErrorException(error))
            case Right(file) =>
              ShaValue.computeShaOf(digestType, file)
          })
        }

      def computeShas(digestType: DigestType, as: NonEmptyList[coursier.Artifact]): Task[ShaValue] = {
        val errorFn: Throwable => Task[ShaValue] = as.tail match {
          case Nil => {e: Throwable =>
            resolverMonad.raiseError(new RuntimeException(s"we could not download the artifact ${c.asString} to compute the hash for digest type ${digestType} with error ${e}"))
          }
          case h :: t => {e: Throwable => computeShas(digestType, NonEmptyList(h, t))}
        }
        resolverMonad.handleErrorWith(computeSha(digestType, as.head))(errorFn)
      }

      def processArtifact(src: Artifact.Source, dep: Dependency, proj: Project): Task[Option[JarDescriptor]] = {
          // TODO, this does not seem like the idea thing, but it seems to work.
          val maybeArtifacts = src.artifacts(dep, proj, None)
            .iterator
            .filter(_.url.endsWith(".jar"))
            .toList

          NonEmptyList.fromList(maybeArtifacts).map { artifacts =>

            val sha1 = downloadShas(DigestType.Sha1, maybeArtifacts.flatMap(_.extra.get("SHA-1"))).flatMap {
              case Some(s) => Task.point(s)
              case None => {
                logger.info(s"Preforming cached fetch to execute SHA-1 calculation for ${artifacts.head}")
                computeShas(DigestType.Sha1, artifacts)
              }
            }

            // Coursier has some artifact data in extra for checksums, but has a far more complete
            // checksumUrls attribute
            //
            // The SHA-1 availability as an artifact appears to be far more complete, so we only do this
            // for the SHA-256
            val sha256Artifacts = maybeArtifacts.flatMap(_.extra.get("SHA-256")) match {
              case Nil =>
                maybeArtifacts.flatMap { a =>
                  a.checksumUrls.get("SHA-256").map{u => a.copy(url = u)}
                }
              case o => o
            }

            val sha256 = downloadShas(DigestType.Sha256, sha256Artifacts).flatMap {
              case Some(s) => Task.point(s)
              case None =>{
                logger.info(s"Preforming cached fetch to execute SHA-256 calculation for ${artifacts.head}")
                computeShas(DigestType.Sha256, artifacts)
              }
            }

            (for {
              sha1 <- sha1
              sha256 <- sha256
            } yield {
              val serverId = serverFor(artifacts.head).fold("")(_.id)

              Some(JarDescriptor(sha1 = Some(sha1),sha256 = Some(sha256),serverId = serverId, url = Some(artifacts.head.url))) : Option[JarDescriptor]
            })
      }.getOrElse(Task.point(Option.empty[JarDescriptor]))
    }

      val module = coursier.Module(c.group.asString, c.artifact.artifactId, Map.empty)
      val version = c.version.asString
      val f = Cache.fetch[Task](checksums = Seq(Some("SHA-1"), Some("SHA-256"), None), cachePolicy = CachePolicy.FetchMissing, pool = CoursierResolver.downloadPool)
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
      Nested[Task, L, ResolvedShasValue](task.flatMap {
        case Left(errors) =>
          val nel = NonEmptyList.fromList(errors.toList)
            .getOrElse(NonEmptyList("<empty message>", Nil))
          Task.point(Validated.invalid(nel))
        case Right((src, proj)) =>
          val dep = coursier.Dependency(module, version, configuration = DefaultConfiguration, attributes = coursier.Attributes(
            c.artifact.packaging,
            c.artifact.classifier.getOrElse("")
          ))

          val srcDep = dep.copy(attributes = coursier.Attributes(
            c.artifact.packaging,
            "sources"
          ))

          processArtifact(src, dep, proj).flatMap { mainJarDescriptorOpt =>
            resolverMonad.handleErrorWith(processArtifact(src, srcDep, proj)){_ => Task.point(None)}.flatMap { sourceJarDescriptorOpt =>
                mainJarDescriptorOpt match {
                case None => resolverMonad.raiseError(new RuntimeException(s"no artifacts for ${c.asString} found")) : Task[ResolvedShasValue]
                case Some(mainJarDescriptor) =>
                  Task.point(ResolvedShasValue(
                    binaryJar = mainJarDescriptor,
                    sourceJar = sourceJarDescriptorOpt
                  ))
                }
            }
          }.map(Validated.valid(_))
      })
    }

    val g: MavenCoordinate => N[(MavenCoordinate, ResolvedShasValue)] =
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
    def toDep(mc: MavenCoordinate): coursier.Dependency = {
      val exs = m.dependencies.excludes(mc.unversioned)
      val exSet: Set[(String, String)] =
        exs.map { elem => (elem.group.asString, elem.artifact.artifactId) }
      coursier.Dependency(
        coursier.Module(mc.group.asString, mc.artifact.artifactId),
        mc.version.asString,
        configuration = DefaultConfiguration,
        exclusions = exSet,
        attributes = coursier.Attributes(
          mc.artifact.packaging,
          mc.artifact.classifier.getOrElse("")
        ))
    }

    def artifactFromDep(cd: coursier.Dependency): MavenArtifactId = {
      val attrs = cd.attributes
      val packaging =
        if (attrs.`type`.isEmpty) "jar"
        else attrs.`type`
      MavenArtifactId(cd.module.name, packaging, attrs.classifier /* empty string OK */)
    }

    def toCoord(cd: coursier.Dependency): MavenCoordinate =
      MavenCoordinate(
        MavenGroup(cd.module.organization),
        artifactFromDep(cd),
        Version(cd.version))

    val roots: Set[coursier.Dependency] = coords.map(toDep).toSet

    Resolution(roots).process.run(fetch).map { res =>
      val depCache = res.finalDependenciesCache

      depCache.foldLeft(Graph.empty[MavenCoordinate, Unit]) { case (g, (n, deps)) =>
        val cnode = toCoord(n)
        val exs = m.dependencies.excludes(cnode.unversioned)
        val g1 = g.addNode(cnode)
        deps.foldLeft(g1) { (g, dep) =>
          val depCoord = toCoord(dep)
          if (dep.optional || exs(depCoord.unversioned)) g
          else g.addEdge(Edge(cnode, depCoord, ()))
        }
      }
    }
  }
}
