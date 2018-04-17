package com.github.johnynek.bazel_deps

import java.security.MessageDigest
import java.io.{ File, FileInputStream }
import java.nio.file.Path
import java.util
import org.apache.maven.repository.internal.MavenRepositorySystemUtils
import org.eclipse.aether.RepositorySystem
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.collection.{ CollectRequest, CollectResult }
import org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory
import org.eclipse.aether.graph.{ Dependency, DependencyNode, DependencyVisitor, Exclusion }
import org.eclipse.aether.impl.DefaultServiceLocator
import org.eclipse.aether.repository.{ LocalRepository, RemoteRepository, RepositoryPolicy }
import org.eclipse.aether.resolution.{ ArtifactResult, ArtifactRequest }
import org.eclipse.aether.spi.connector.RepositoryConnectorFactory
import org.eclipse.aether.spi.connector.transport.TransporterFactory
import org.eclipse.aether.transport.file.FileTransporterFactory
import org.eclipse.aether.transport.http.HttpTransporterFactory
import org.slf4j.LoggerFactory
import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal
import scala.collection.immutable.SortedMap
import scala.collection.breakOut
import cats.{catsInstancesForId, Id, Monad}

class AetherResolver(servers: List[MavenServer], resolverCachePath: Path) extends SequentialResolver[Id] {

  private[this] val logger = LoggerFactory.getLogger(getClass)

  logger.info(s"using resolverCachePath: $resolverCachePath")
  servers.foreach { case MavenServer(id, _, url) =>
    logger.info(s"using resolver $id -> $url")
  }

  def resolverMonad: Monad[Id] = catsInstancesForId

  private val system = {
    val locator = MavenRepositorySystemUtils.newServiceLocator
    locator.addService(classOf[RepositoryConnectorFactory], classOf[BasicRepositoryConnectorFactory])
    locator.addService(classOf[TransporterFactory], classOf[FileTransporterFactory])
    locator.addService(classOf[TransporterFactory], classOf[HttpTransporterFactory])

    locator.setErrorHandler(new DefaultServiceLocator.ErrorHandler {
      override def serviceCreationFailed(t: Class[_], impl: Class[_], exception: Throwable) {
        exception.printStackTrace()
      }
    })

    locator.getService(classOf[RepositorySystem])
  }

  private val session = {
    val s = MavenRepositorySystemUtils.newSession()
    val localRepo = new LocalRepository(resolverCachePath.toString)
    s.setLocalRepositoryManager(system.newLocalRepositoryManager(s, localRepo))
    s
  }

  private val repositories =
    servers.map { case MavenServer(id, t, u) =>
      new RemoteRepository.Builder(id, t, u)
        // Disable warnings from bazel-deps not passing checksums to Aether.  Use the default update policy.
        .setPolicy(new RepositoryPolicy(true, RepositoryPolicy.UPDATE_POLICY_DAILY, RepositoryPolicy.CHECKSUM_POLICY_IGNORE))
        .build
    }.asJava

  /**
   * Here is where the IO happens
   */
  private def request(m: MavenCoordinate, ml: Model): CollectResult = {
    val collectRequest = new CollectRequest()
    val ex = ml.dependencies.excludes(m.unversioned)
    val exclusions = new util.ArrayList[Exclusion]()
    for (elem <- ex){
      val exclusion = new Exclusion(elem.group.asString, elem.artifact.asString, "", "jar")
      exclusions.add(exclusion)
    }
    collectRequest.setRoot(new Dependency(new DefaultArtifact(m.asString), "", false, exclusions))
    collectRequest.setRepositories(repositories)
    system.collectDependencies(session, collectRequest);
  }

  def getShas(m: List[MavenCoordinate]): SortedMap[MavenCoordinate, Try[ResolvedSha1Value]] = {
    /**
     * We try to request the jar.sha1 file, if that fails, we request the jar
     * and do the sha1.
     */
    def toArtifactRequest(m: MavenCoordinate, extension: String): ArtifactRequest = {
      val classifier = null // We don't use this
      val art = new DefaultArtifact(
        m.group.asString, m.artifact.asString, classifier, extension, m.version.asString)
      val context = null
      new ArtifactRequest(art, repositories, context)
    }

    def liftKeys[K: Ordering, V](ms: Iterable[K],
      tmap: Try[Map[K, Try[V]]]): SortedMap[K, Try[V]] =
      ms.map { coord => coord -> tmap.flatMap(_(coord)) }(breakOut)

    def getExt(ms: Seq[MavenCoordinate], ext: String)(toSha: File => Try[Sha1Value]): SortedMap[MavenCoordinate, Try[ResolvedSha1Value]] =
      liftKeys(ms, Try {
        val resp =
          system.resolveArtifacts(session,
            ms.map(toArtifactRequest(_, ext)).toList.asJava)
            .asScala
            .iterator

        ms.iterator.zip(resp).map { case (coord, r) =>
          coord -> getFile(coord, ext, r).flatMap(f => toSha(f).map(sha1Value => ResolvedSha1Value(sha1Value, r.getRepository.getId)))
        }.toMap
      })

    val shas = getExt(m.toList, "jar.sha1")(readShaContents)
    val computes =
      getExt(shas.collect { case (m, Failure(_)) => m }.toList, "jar")(computeShaOf)

    shas ++ computes
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

  private def readShaContents(f: File): Try[Sha1Value] =
    Model.readFile(f).flatMap { str =>
      val hexString = str
          .split("\\s") // some files have sha<whitespace>filename
          .dropWhile(_.isEmpty)
          .head
          .trim
          .toLowerCase
      if (hexString.length == 40 && hexString.matches("[0-9A-Fa-f]*")) {
        Success(Sha1Value(hexString))
      } else {
        Failure(new Exception(s"string: $hexString, not a valid SHA1"))
      }
    }

  private def computeShaOf(f: File): Try[Sha1Value] = Try {
    val sha = MessageDigest.getInstance("SHA-1")
    val fis = new FileInputStream(f)
    try {
      var n = 0;
      val buffer = new Array[Byte](8192)
      while (n != -1) {
        n = fis.read(buffer)
        if (n > 0) sha.update(buffer, 0, n)
      }
      Success(Sha1Value(sha.digest.map("%02X".format(_)).mkString.toLowerCase))
    }
    catch {
      case NonFatal(err) => Failure(err)
    }
    finally {
      fis.close
    }
  }.flatten

  type Node = MavenCoordinate

  def addToGraph(deps: Graph[Node, Unit], dep: MavenCoordinate, m: Model): Graph[Node, Unit] = {
    val visitor = new Visitor(deps, m)
    val result = request(dep, m).getRoot.accept(visitor)
    visitor.currentDeps
  }

  private class Visitor(initDeps: Graph[Node, Unit], model: Model) extends DependencyVisitor {
    var currentDeps = initDeps
    private var visited: Set[(Dependency, Boolean)] = Set.empty
    private var stack: List[Dependency] = Nil

    def coord(a: Dependency): MavenCoordinate = {
      val artifact = a.getArtifact
      MavenCoordinate(MavenGroup(artifact.getGroupId),
        MavenArtifactId(artifact.getArtifactId),
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
        stack match {
          case Nil => ()
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
      true
    }
  }
}
