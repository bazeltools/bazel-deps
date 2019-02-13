package com.github.johnynek.bazel_deps

import java.io.File
import java.net.URI
import java.nio.file.Path
import java.util
import org.apache.maven.repository.internal.MavenRepositorySystemUtils
import org.apache.maven.settings.Server
import org.eclipse.aether.RepositorySystem
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.collection.{ CollectRequest, CollectResult }
import org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory
import org.eclipse.aether.graph.{ Dependency, DependencyNode, DependencyVisitor, Exclusion }
import org.eclipse.aether.impl.DefaultServiceLocator
import org.eclipse.aether.internal.impl.Maven2RepositoryLayoutFactory
import org.eclipse.aether.repository.{ LocalRepository, RemoteRepository, RepositoryPolicy }
import org.eclipse.aether.resolution.{ ArtifactResult, ArtifactRequest }
import org.eclipse.aether.spi.connector.RepositoryConnectorFactory
import org.eclipse.aether.spi.connector.transport.TransporterFactory
import org.eclipse.aether.transport.file.FileTransporterFactory
import org.eclipse.aether.transport.http.HttpTransporterFactory
import org.eclipse.aether.util.repository.AuthenticationBuilder
import org.slf4j.LoggerFactory
import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}
import scala.collection.immutable.SortedMap
import scala.collection.breakOut
import cats.{instances, MonadError, Foldable}

import cats.implicits._

class AetherResolver(servers: List[MavenServer], resolverCachePath: Path) extends SequentialResolver[Try] {

  private[this] val logger = LoggerFactory.getLogger(getClass)

  logger.info(s"using resolverCachePath: $resolverCachePath")
  servers.foreach { case MavenServer(id, _, url) =>
    logger.info(s"using resolver $id -> $url")
  }

  def run[A](a: Try[A]): Try[A] = a
  def resolverMonad: MonadError[Try, Throwable] = instances.try_.catsStdInstancesForTry

  private val system: RepositorySystem = {
    val locator = MavenRepositorySystemUtils.newServiceLocator
    locator.addService(classOf[RepositoryConnectorFactory], classOf[BasicRepositoryConnectorFactory])
    locator.addService(classOf[TransporterFactory], classOf[FileTransporterFactory])
    locator.addService(classOf[TransporterFactory], classOf[HttpTransporterFactory])

    locator.setErrorHandler(new DefaultServiceLocator.ErrorHandler {
      override def serviceCreationFailed(t: Class[_], impl: Class[_], exception: Throwable) {
        logger.error(s"could not create service: $t, $impl", exception)
        exception.printStackTrace()
      }
    })

    locator.getService(classOf[RepositorySystem])
  }

  private val session = {
    val s = MavenRepositorySystemUtils.newSession()
    val localRepo = new LocalRepository(resolverCachePath.toString)
    s.setLocalRepositoryManager(system.newLocalRepositoryManager(s, localRepo))
    s.setIgnoreArtifactDescriptorRepositories(true)
    s
  }

  private val repositories = {
    val settings = SettingsLoader.settings

    servers.map { case MavenServer(id, t, u) =>
      // If there are no credentials for server present, we can just pass in nulls
      val server = Option(settings.getServer(id)).getOrElse(new Server)

      new RemoteRepository.Builder(id, t, u)
        // Disable warnings from bazel-deps not passing checksums to Aether.  Use the default update policy.
        .setPolicy(new RepositoryPolicy(true, RepositoryPolicy.UPDATE_POLICY_DAILY, RepositoryPolicy.CHECKSUM_POLICY_IGNORE))
        .setAuthentication(new AuthenticationBuilder()
          .addUsername(server.getUsername)
          .addPassword(server.getPassword)
          .build())
        .build
    }.asJava
  }

  /**
   * Here is where the IO happens
   */
  private def request(m: MavenCoordinate, ml: Model): CollectResult = {
    val collectRequest = new CollectRequest()
    val ex = ml.dependencies.excludes(m.unversioned)
    val exclusions = new util.ArrayList[Exclusion]()
    for (elem <- ex){
      val exclusion = new Exclusion(
        elem.group.asString,
        elem.artifact.artifactId,
        elem.artifact.classifier.orNull,
        elem.artifact.packaging)
      exclusions.add(exclusion)
    }
    collectRequest.setRoot(new Dependency(new DefaultArtifact(m.asString), "", false, exclusions))
    collectRequest.setRepositories(repositories)
    system.collectDependencies(session, collectRequest)
  }

  def getShas(m: List[MavenCoordinate]): Try[SortedMap[MavenCoordinate, ResolvedShasValue]] = {
    /**
     * We try to request the jar.sha1 file, if that fails, we request the jar
     * and do the sha1.
     */
    def toArtifactRequest(m: MavenCoordinate, extensionSuffix: String): ArtifactRequest = {
      val a = m.artifact
      val art = new DefaultArtifact(
        m.group.asString,
        a.artifactId,
        a.classifier.orNull,
        a.packaging + extensionSuffix /* e.g. "jar" + .sha" */,
        m.version.asString)
      val context = null
      new ArtifactRequest(art, repositories, context)
    }

    def liftKeys[K: Ordering, V](ms: Iterable[K],
      tmap: Try[Map[K, Try[V]]]): SortedMap[K, Try[V]] =
      ms.map { coord => coord -> tmap.flatMap(_(coord)) }(breakOut)

    def getExt(ms: Seq[MavenCoordinate], ext: String)(toSha: File => Try[ShaValue]): SortedMap[MavenCoordinate, Try[JarDescriptor]] =
      liftKeys(ms, Try {
        val resp =
          system.resolveArtifacts(session,
            ms.map(toArtifactRequest(_, ext)).toList.asJava)
            .asScala
            .iterator

        ms.iterator.zip(resp).map { case (coord, r) => coord -> {
          val remoteRepository = r.getRepository.asInstanceOf[RemoteRepository]
          val repositoryLayout = new Maven2RepositoryLayoutFactory().newInstance(session, remoteRepository)

          for {
            f <- getFile(coord, ext, r)
            sha1 <- ShaValue.computeShaOf(DigestType.Sha1, f)
            sha256 <- ShaValue.computeShaOf(DigestType.Sha256, f)
          } yield {
            JarDescriptor(
              url = Some(new URI(remoteRepository.getUrl).resolve(repositoryLayout.getLocation(r.getArtifact, false).toString).toString),
              sha1 = Some(sha1),
              sha256 = Some(sha256),
              serverId = r.getRepository.getId
            )
          }}}.toMap
      })

    val shas = getExt(m.toList, "sha1")(readShaContents)
    val computes =
      getExt(shas.collect { case (m, Failure(_)) => m }.toList, "" /* no suffix */)(ShaValue.computeShaOf(DigestType.Sha1,_))

    // this is sequence but this version of cats does not have traverse on SortedMap
    Foldable[List].foldM(
      (shas ++ computes).toList,
      SortedMap.empty[MavenCoordinate, ResolvedShasValue]) { case (m, (k, trySha)) =>
        trySha.map { sha => m + (k ->
          ResolvedShasValue(binaryJar = sha, sourceJar = None)) }
      }
  }
  private def getFile(m: MavenCoordinate, ext: String, a: ArtifactResult): Try[File] =
    a.getArtifact match {
      case null => Failure(ResolveFailure("null artifact", m, ext, a.getExceptions.asScala.toList))
      case art =>
        val f = art.getFile
        if (f == null) {
          Failure(ResolveFailure("null file", m, ext, a.getExceptions.asScala.toList))
        }
        else Success(f)
    }

  private def readShaContents(f: File): Try[ShaValue] =
    Model.readFile(f).flatMap(ShaValue.parseData(DigestType.Sha1, _))

  type Node = MavenCoordinate

  def addToGraph(deps: Graph[Node, Unit], dep: MavenCoordinate, m: Model): Try[Graph[Node, Unit]] = Try {
    val collectResult = request(dep, m)
    val exceptions = collectResult.getExceptions.asScala
    if (exceptions.nonEmpty) {
      logger.error(s"exceptions on request: ${exceptions.toList}")
    }
    val visitor = new Visitor(deps, m)
    collectResult.getRoot.accept(visitor)
    visitor.currentDeps
  }

  private class Visitor(initDeps: Graph[Node, Unit], model: Model) extends DependencyVisitor {
    var currentDeps = initDeps
    private var visited: Set[(Dependency, Boolean)] = Set.empty
    private var stack: List[Dependency] = Nil

    def coord(a: Dependency): MavenCoordinate = {
      val artifact = a.getArtifact
      MavenCoordinate(MavenGroup(artifact.getGroupId),
        MavenArtifactId(
          artifact.getArtifactId /* non-null */,
          artifact.getExtension /* non-null; corresponds to "packaging" */,
          artifact.getClassifier /* non-null; "" -> no classifier */
        ),
        Version(artifact.getVersion))
    }

    def addEdgeTo(d: Dependency): Boolean =
      (!d.isOptional) &&
      (d.getScope.toLowerCase match {
        case "" => true // default
        case "compile" => true // default
        case "provided" => false // TODO: we will need to revisit this
        case "runtime" => true // TODO: we should only add these to runtime deps
        case "test" => false
        case "system" => false // these should not be in maven, and should be handled by replacements
        case "import" =>
          // This means pull all the dependencies from a pom we are pointing to
          sys.error("unsupported")
        case other => sys.error(s"unknown scope: $other in $d")
      })

    /**
     * Some maven artifacts are replaced, meaning we deal with them and
     * their dependencies manually. If this is true, never follow (but
     * we do add the edges to the node in such cases
     */
    def notReplaced(m: MavenCoordinate): Boolean =
      model.getReplacements
        .get(m.unversioned)
        .isEmpty

    def excludeEdge(src: MavenCoordinate, dest: MavenCoordinate): Boolean =
      model.dependencies.excludes(src.unversioned).contains(dest.unversioned)

    def visitEnter(depNode: DependencyNode): Boolean = {
      logger.info(s"${depNode.getDependency} -> ${depNode.getChildren.asScala.toList.map(_.getDependency)}")
      val dep = depNode.getDependency
      val shouldAdd = addEdgeTo(dep)
      /**
       * unfollowed nodes are distinct from followed nodes.
       * If project a has an optional dependency on b, that does
       * not mean another project does not have a non-optional dependency
       */
      if (visited((dep, shouldAdd))) {
        logger.info(s"already seen dep: ($dep, $shouldAdd)")
        false
      } else {
        visited = visited + (dep -> shouldAdd)
        val mvncoord = coord(dep)
        if (shouldAdd) {
          logger.info(s"adding dep: ($dep, ${dep.isOptional}, ${dep.getScope})")
          currentDeps = currentDeps.addNode(mvncoord)
        } else {
          logger.info(s"not adding dep: ($dep, ${dep.isOptional}, ${dep.getScope})")
        }
        logger.info(s"path depth: ${stack.size}")
        stack match {
          case Nil =>
            ()
          case h :: _ =>
            val src = coord(h)
            if (shouldAdd && !excludeEdge(src, mvncoord)) {
              logger.info(s"adding edge: $src -> $mvncoord")
              currentDeps = currentDeps.addEdge(Edge(src, mvncoord, ()))
            } else {
              logger.info(s"not adding edge: $src -> $mvncoord")
            }
        }
        stack = dep :: stack
        shouldAdd && notReplaced(mvncoord)
      }
    }
    def visitLeave(dep: DependencyNode): Boolean = {
      require(stack.head == dep.getDependency, s"stack mismatch: ${stack.head} != ${dep.getDependency}")
      stack = stack.tail
      true // always visit siblings
    }
  }
}
