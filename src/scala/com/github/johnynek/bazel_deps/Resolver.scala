package com.github.johnynek.bazel_deps

import java.io.{ File, BufferedReader, FileReader, FileInputStream }
import java.nio.CharBuffer
import java.security.MessageDigest
import org.apache.maven.repository.internal.MavenRepositorySystemUtils
import org.eclipse.aether.RepositorySystem
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.collection.{ CollectRequest, CollectResult }
import org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory
import org.eclipse.aether.graph.{ Dependency, DependencyNode, DependencyVisitor }
import org.eclipse.aether.impl.DefaultServiceLocator
import org.eclipse.aether.repository.{ LocalRepository, RemoteRepository }
import org.eclipse.aether.resolution.{ ArtifactResult, ArtifactRequest }
import org.eclipse.aether.spi.connector.RepositoryConnectorFactory
import org.eclipse.aether.spi.connector.transport.TransporterFactory
import org.eclipse.aether.transport.file.FileTransporterFactory
import org.eclipse.aether.transport.http.HttpTransporterFactory
import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

case class ResolveFailure(message: String,
  m: MavenCoordinate,
  extension: String,
  failures: List[Exception]) extends Exception(message)

class Resolver(servers: List[MavenServer]) {

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
    val localRepo = new LocalRepository("target/local-repo")
    s.setLocalRepositoryManager(system.newLocalRepositoryManager(s, localRepo))
    s
  }

  private val repositories =
    servers.map { case MavenServer(id, t, u) =>
      new RemoteRepository.Builder(id, t, u).build
    }.asJava

  /**
   * Here is where the IO happens
   */
  private def request(m: MavenCoordinate): CollectResult = {
    val collectRequest = new CollectRequest()
    collectRequest.setRoot(new Dependency(new DefaultArtifact(m.asString), ""))
    collectRequest.setRepositories(repositories)
    system.collectDependencies(session, collectRequest);
  }

  def getShas(m: Iterable[MavenCoordinate]): Map[MavenCoordinate, Try[Sha1Value]] = {
    /**
     * We try to request the .jar.sha1 file, if that fails, we request the .jar
     * and do the sha1.
     */
    def toArtifactRequest(m: MavenCoordinate, extension: String): ArtifactRequest = {
      val classifier = null // We don't use this
      val art = new DefaultArtifact(
        m.group.asString, m.artifact.asString, classifier, extension, m.version.asString)
      val context = null
      new ArtifactRequest(art, repositories, context)
    }

    def liftKeys[K, V](ms: Iterable[K],
      tmap: Try[Map[K, Try[V]]]): Map[K, Try[V]] =
      ms.map { coord => coord -> tmap.flatMap(_(coord)) }.toMap

    def getExt(ms: Seq[MavenCoordinate], ext: String)(toSha: File => Try[Sha1Value]): Map[MavenCoordinate, Try[Sha1Value]] =
      liftKeys(ms, Try {
        val resp =
          system.resolveArtifacts(session,
            ms.map(toArtifactRequest(_, ext)).toList.asJava)
            .asScala
            .iterator

        ms.iterator.zip(resp).map { case (coord, r) =>
          coord -> getFile(coord, ext, r).flatMap(toSha)
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

  private def readShaContents(f: File): Try[Sha1Value] = Try {
    val fr = new FileReader(f)
    val buf = new BufferedReader(fr)
    try {
      val bldr = new StringBuilder
      val cbuf = new Array[Char](1024)
      var read = 0
      while(read >= 0) {
        read = buf.read(cbuf, 0, 1024)
        // somehow scala sees all the append methods and picks the wrong one unless we force it
        val charbuf = CharBuffer.wrap(cbuf, 0, read)
        if (read > 0) bldr.append(charbuf: CharSequence)
      }
      Success(Sha1Value(bldr.toString.trim.toLowerCase))
    }
    catch {
      case NonFatal(err) => Failure(err)
    }
    finally {
      fr.close
    }
  }.flatten

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

  def addAll(deps: Graph[Node, Unit], coords: TraversableOnce[MavenCoordinate], r: Replacements): Graph[Node, Unit] =
    coords.foldLeft(deps)(addToGraph(_, _, r))

  def addToGraph(deps: Graph[Node, Unit], dep: MavenCoordinate, r: Replacements): Graph[Node, Unit] = {
    val visitor = new Visitor(deps, r)
    val result = request(dep).getRoot.accept(visitor)
    visitor.currentDeps
  }

  private class Visitor(initDeps: Graph[Node, Unit], r: Replacements) extends DependencyVisitor {
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
        case "provided" => true // TODO: we will need to revisit this
        case "runtime" => true // TODO: we should only add these to runtime deps
        case "test" => false
        case "system" => false // these should not be in maven, and should be handled by replacements
        case "import" =>
          // This means pull all the dependencies from a pom we are pointing to
          sys.error("unsupported")
        case other => sys.error(s"unknown scope: $other in $d")
      })

    def visitEnter(depNode: DependencyNode): Boolean = {
      val dep = depNode.getDependency
      val shouldAdd = addEdgeTo(dep)
      /**
       * unfollowed nodes are distinct from followed nodes.
       * If project a has an optional dependency on b, that does
       * not mean another project does not have a non-optional dependency
       */
      if (visited((dep, shouldAdd))) false
      else {
        visited = visited + (dep -> shouldAdd)
        val mvncoord = coord(dep)
        if (shouldAdd) {
          currentDeps = currentDeps.addNode(mvncoord)
        }
        else {
          //println(s"$dep, ${dep.isOptional}, ${dep.getScope}")
        }
        stack match {
          case Nil => ()
          case h :: _ =>
            if (shouldAdd) {
              currentDeps = currentDeps
                .addEdge(Edge(coord(h), mvncoord, ()))
            }
        }
        stack = dep :: stack
        /**
         * Some maven artifacts are replaced, meaning we deal with them and
         * their dependencies manually. If this is true, never follow (but
         * we do add the edges to the node in such cases
         */
        shouldAdd && r.get(mvncoord.unversioned).isEmpty
      }
    }
    def visitLeave(dep: DependencyNode): Boolean = {
      require(stack.head == dep.getDependency, s"stack mismatch: ${stack.head} != ${dep.getDependency}")
      stack = stack.tail
      true
    }
  }
}
