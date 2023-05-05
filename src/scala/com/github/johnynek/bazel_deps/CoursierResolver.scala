package com.github.johnynek.bazel_deps

import coursier.{Dependency, ResolutionProcess, Project, Resolution}
import coursier.cache.{FileCache, CachePolicy}
import coursier.util.{Artifact, Task}
import cats.MonadError
import cats.data.{Nested, NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import coursier.LocalRepositories
import coursier.core._
import java.nio.file.Path
import org.slf4j.LoggerFactory
import coursier.ivy._
import scala.collection.immutable.SortedMap
import scala.util.{Failure, Try}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

object CoursierResolver {
  // 12 concurrent downloads
  // most downloads are tiny sha downloads so try keep things alive
  lazy val downloadPool = {
    import java.util.concurrent.{ExecutorService, Executors, ThreadFactory}
    Executors.newFixedThreadPool(
      12,
      // from scalaz.concurrent.Strategy.DefaultDaemonThreadFactory
      new ThreadFactory {
        val defaultThreadFactory = Executors.defaultThreadFactory()

        def newThread(r: Runnable) = {
          val t = defaultThreadFactory.newThread(r)
          t.setDaemon(true)
          t
        }
      }
    )
  }
}

class CoursierResolver(servers: List[DependencyServer], ec: ExecutionContext, runTimeout: Duration, resolverCachePath: Path) extends Resolver[Task] {
  // TODO: add support for a local file cache other than ivy
  private[this] val repos = LocalRepositories.ivy2Local :: {
    val settings = SettingsLoader.settings

    servers.flatMap {
      case MavenServer(id, _, url) =>
        val authentication = Option(settings.getServer(id))
          .map(server => Authentication(server.getUsername, server.getPassword))

        coursier.MavenRepository(url, authentication = authentication) :: Nil

      case is @ IvyServer(id, url, ivyPattern, ivyArtifactPattern) =>
        val authentication = Option(settings.getServer(id))
          .map(server => Authentication(server.getUsername, server.getPassword))

        IvyRepository.parse(
          url + ivyArtifactPattern,
          Some(url + ivyPattern),
          authentication = authentication
        ) match {
          case Left(o)  =>
            System.err.println(s"ignoring $is due to parse error:\n\n\t$o\n")
            Nil
          case Right(r) => r :: Nil
        }
    }
  }

  private[this] def makeCache() = 
    Option(resolverCachePath) match {
      case None => FileCache()
      case Some(p) => FileCache().withLocation(p.toAbsolutePath.toString)
    }
  private[this] val fetch = ResolutionProcess.fetch(repos,
    makeCache()
      .withCachePolicies(Seq(CachePolicy.FetchMissing))
      .withPool(CoursierResolver.downloadPool)
      .fetch)

  private[this] val logger =
    LoggerFactory.getLogger("bazel_deps.CoursierResolver")

  // Instructs the coursier resolver to keep `runtime`-scoped dependencies.
  private[this] val DefaultConfiguration = "default(compile)"

  def serverFor(a: Artifact): Option[DependencyServer] =
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

  case class FileErrorException(error: coursier.cache.ArtifactError)
      extends Exception(error.describe)

  case class DownloadFailures(messages: NonEmptyList[String])
      extends Exception("resolution errors:\n" + messages.toList.mkString("\n"))

  def getShas(
      m: List[MavenCoordinate]
  ): Task[SortedMap[MavenCoordinate, ResolvedShasValue]] = {

    type L[x] = ValidatedNel[String, x]
    type N[x] = Nested[Task, L, x]

    def lookup(c: MavenCoordinate): N[ResolvedShasValue] = {
      def computeSha(
          digestType: DigestType,
          artifact: Artifact
      ): Task[(Artifact, ShaValue, Long)] = {
        makeCache()
          .withCachePolicies(Seq(CachePolicy.FetchMissing))
          .withPool(CoursierResolver.downloadPool)
          .file(artifact)
          .run
          .flatMap { e =>
            resolverMonad.fromTry(e match {
              case Left(error) =>
                // println(s"Tried to download $artifact but failed.")
                Failure(FileErrorException(error))
              case Right(file) =>
                ShaValue.computeShaOf(digestType, file.toPath).map { sha =>
                  (artifact, sha, file.length())
                }
            })
          }
      }

      def computeShas(
          digestType: DigestType,
          as: NonEmptyList[Artifact]
      ): Task[(Artifact, ShaValue, Long)] = {
        val errorFn: Throwable => Task[(Artifact, ShaValue, Long)] = as.tail match {
          case Nil => { e: Throwable =>
            resolverMonad.raiseError(
              new RuntimeException(
                s"we could not download the artifact ${c.asString} to compute the hash for digest type ${digestType} with error ${e}"
              )
            )
          }
          case h :: t => { e: Throwable =>
            computeShas(digestType, NonEmptyList(h, t))
          }
        }
        resolverMonad.handleErrorWith(computeSha(digestType, as.head))(errorFn)
      }

      def processArtifact(
          src: coursier.core.ArtifactSource,
          dep: Dependency,
          proj: Project
      ): Task[Option[JarDescriptor]] = {
        val module = dep.module
        val organization = module.organization.value
        val moduleName = module.name.value
        val version = dep.version
        val extension = dep.publication.ext.value
        val classifier = Option(dep.publication.classifier.value).filter(_.nonEmpty).filter(_ != "sources")
        // sometimes the artifactor source doesn't seem to entirely work... so
        // we inject using any ivy servers about test URL's to try
        val extraUrls = servers.collect {
          case IvyServer(_, url, _, ivyArtifactPattern) =>
            val subUrl = ivyArtifactPattern
              .replaceAllLiterally("[revision]", version)
              .replaceAllLiterally("[orgPath]", organization.replace('.', '/'))
              .replaceAllLiterally("[artifact]", moduleName)
              .replaceAllLiterally("[module]", moduleName)
              .replaceAllLiterally("(-[classifier])", classifier.getOrElse(""))
              .replaceAllLiterally(
                "[ext]",
                Option(extension).filter(_.nonEmpty).getOrElse("jar")
              )

            Some(s"$url$subUrl")
          case MavenServer(_, _, url) =>
            // Builds a Maven artifact URL
            def mavenUrl(
                url: String,
                organization: String,
                moduleName: String,
                version: String,
                classifier: Option[String],
                extension: Option[String]
            ): String = {
              val classifierSuffix: String =
                classifier.filter(_.nonEmpty).map("-" + _).getOrElse("")
              val ext: String = extension.filter(_.nonEmpty).getOrElse("jar")

              s"${url.stripSuffix("/")}/${organization.replace('.', '/')}/$moduleName/$version/$moduleName-$version$classifierSuffix.$ext"
            }

            Some(
              mavenUrl(
                url,
                organization,
                moduleName,
                version,
                classifier,
                Option(extension)
              )
            )
        }.flatten

        val maybeArtifacts = src
          .artifacts(dep, proj, None)
          .map { case (_, artifact: Artifact) => artifact }
          .toList ++ extraUrls.map { url =>
          Artifact(
            url,
            Map.empty,
            Map.empty,
            false,
            false,
            None
          )
        }

        if (maybeArtifacts == Nil) {
          logger.warn(s"Failed to process $dep")
        }

        NonEmptyList
          .fromList(maybeArtifacts)
          .map { artifacts =>
            for {
              // No artifacts actually have Sha256's available
              // so don't bother trying to fetch anything.
              // Once we download the jar at all, calculating sha's is ~cheap.
              foundSha1Data <- computeShas(DigestType.Sha1, artifacts)
              (sha1Artifact, sha1, _) = foundSha1Data
              foundShaData <- computeShas(DigestType.Sha256, artifacts)
              (artifact, sha256, fileSizeBytes) = foundShaData
            } yield {
              val serverId = serverFor(artifact).fold("")(_.id)

              Some(
                JarDescriptor(
                  sha1 = Some(sha1),
                  sha256 = Some(sha256),
                  fileSizeBytes = Some(fileSizeBytes),
                  serverId = serverId,
                  url = Some(artifact.url)
                )
              ): Option[JarDescriptor]
            }
          }
          .getOrElse(Task.point(Option.empty[JarDescriptor]))
      }

      val module = coursier.Module(
        Organization(c.group.asString),
        ModuleName(c.artifact.artifactId),
        Map.empty
      )
      val version = c.version.asString
      val f = makeCache()
        .withChecksums(Seq(Some("SHA-1"), None))
        .withCachePolicies(Seq(CachePolicy.FetchMissing))
        .withPool(CoursierResolver.downloadPool)
        .fetch
      val task =
        ResolutionProcess.fetchOne[Task](repos, module, version, f, Seq()).run

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
          val nel = NonEmptyList
            .fromList(errors.toList)
            .getOrElse(NonEmptyList("<empty message>", Nil))
          Task.point(Validated.invalid(nel))
        case Right((src, proj)) =>
          val dep = coursier
            .Dependency(module, version)
            .withConfiguration(Configuration(DefaultConfiguration))
            .withAttributes(
              coursier.Attributes(
                Type(c.artifact.packaging),
                Classifier(c.artifact.classifier.getOrElse(""))
              )
            )

          val srcDep = dep.withAttributes(
            coursier.Attributes(
              Type(c.artifact.packaging),
              Classifier("sources")
            )
          )

          processArtifact(src, dep, proj)
            .flatMap { mainJarDescriptorOpt =>
              resolverMonad
                .handleErrorWith(processArtifact(src, srcDep, proj)) { _ =>
                  Task.point(None)
                }
                .flatMap { sourceJarDescriptorOpt =>
                  mainJarDescriptorOpt match {
                    case None =>
                      resolverMonad.raiseError(
                        new RuntimeException(
                          s"no artifacts for ${c.asString} found. src: $src, dep: $dep, proj: $proj"
                        )
                      ): Task[ResolvedShasValue]
                    case Some(mainJarDescriptor) =>
                      Task.point(
                        ResolvedShasValue(
                          binaryJar = mainJarDescriptor,
                          sourceJar = sourceJarDescriptorOpt
                        )
                      )
                  }
                }
            }
            .map(Validated.valid(_))
      })
    }

    val g: MavenCoordinate => N[(MavenCoordinate, ResolvedShasValue)] =
      x => lookup(x).map(x -> _)

    Task.gather.gather(m.map(x => g(x).value)).map(_.toList.sequence).flatMap {
      case Validated.Valid(xs) =>
        Task.point(SortedMap(xs: _*))
      case Validated.Invalid(errors) =>
        resolverMonad.raiseError(DownloadFailures(errors))
    }
  }

  // Build the entire transitive graph of a set of coordinates
  def buildGraph(
      coords: List[MavenCoordinate],
      m: Model
  ): Task[Graph[MavenCoordinate, Unit]] = {
    def toDep(mc: MavenCoordinate): coursier.Dependency = {
      val exs = m.dependencies.excludes(mc.unversioned)
      val exSet: Set[(Organization, ModuleName)] =
        exs.map { elem =>
          (
            Organization(elem.group.asString),
            ModuleName(elem.artifact.artifactId)
          )
        }
      coursier
        .Dependency(
          coursier.Module(
            Organization(mc.group.asString),
            ModuleName(mc.artifact.artifactId)
          ),
          mc.version.asString
        )
        .withConfiguration(Configuration(DefaultConfiguration))
        .withExclusions(exSet)
        .withAttributes(
          coursier.Attributes(
            Type(mc.artifact.packaging),
            Classifier(mc.artifact.classifier.getOrElse(""))
          )
        )
    }

    def artifactFromDep(cd: coursier.Dependency): MavenArtifactId = {
      val attrs = cd.attributes
      val packaging =
        if (attrs.`type`.isEmpty) "jar"
        else attrs.`type`.value
      MavenArtifactId(
        cd.module.name.value,
        packaging,
        attrs.classifier.value /* empty string OK */
      )
    }

    def toCoord(cd: coursier.Dependency): MavenCoordinate =
      MavenCoordinate(
        MavenGroup(cd.module.organization.value),
        artifactFromDep(cd),
        Version(cd.version)
      )

    val rootsSet = coords.map(toDep).toSet
    val roots: Seq[coursier.core.Dependency] = rootsSet.toSeq

    Resolution(roots).process.run(fetch).map { res =>
      val depCache = res.finalDependenciesCache

      if (res.errors.nonEmpty) {
        res.errors.foreach { case (_, msgs) => msgs.foreach(logger.error) }
        throw new RuntimeException("Failed to resolve dependencies")
      }

      depCache.foldLeft(Graph.empty[MavenCoordinate, Unit]) {
        case (g, (n, deps)) =>
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
