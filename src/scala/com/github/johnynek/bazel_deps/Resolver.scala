package com.github.johnynek.bazel_deps

import org.apache.maven.repository.internal.MavenRepositorySystemUtils
import org.eclipse.aether.RepositorySystem
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.collection.CollectRequest
import org.eclipse.aether.collection.CollectResult
import org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory
import org.eclipse.aether.graph.Dependency
import org.eclipse.aether.graph.DependencyNode
import org.eclipse.aether.graph.DependencyVisitor
import org.eclipse.aether.impl.DefaultServiceLocator
import org.eclipse.aether.repository.LocalRepository
import org.eclipse.aether.repository.RemoteRepository
import org.eclipse.aether.spi.connector.RepositoryConnectorFactory
import org.eclipse.aether.spi.connector.transport.TransporterFactory
import org.eclipse.aether.transport.file.FileTransporterFactory
import org.eclipse.aether.transport.http.HttpTransporterFactory
import scala.collection.JavaConverters._

case class MavenServer(id: String, contentType: String, url: String)

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

  type Node = MavenCoordinate

  def addAll(deps: Graph[Node, Unit], coords: TraversableOnce[MavenCoordinate]): Graph[Node, Unit] =
    coords.foldLeft(deps)(addToGraph)

  def addToGraph(deps: Graph[Node, Unit], dep: MavenCoordinate): Graph[Node, Unit] = {
    val visitor = new Visitor(deps)
    val result = request(dep).getRoot.accept(visitor)
    visitor.currentDeps
  }

  private class Visitor(initDeps: Graph[Node, Unit]) extends DependencyVisitor {
    var currentDeps = initDeps
    var visited: Set[Dependency] = Set.empty
    var stack: List[Dependency] = Nil

    def coord(a: Dependency): MavenCoordinate = {
      val artifact = a.getArtifact
      MavenCoordinate(MavenGroup(artifact.getGroupId),
        MavenArtifactId(ArtifactOrProject(artifact.getArtifactId), None),
        Version(artifact.getVersion))
    }

    def follow(d: Dependency): Boolean =
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
      if (visited(dep)) false
      else {
        visited = visited + dep
        val f = follow(dep)
        stack match {
          case Nil => ()
          case h :: _ =>
            // this is an implicit dependency
            if (f) {
              currentDeps = currentDeps
                .addEdge(Edge(coord(h), coord(dep), ()))
            }
        }
        stack = dep :: stack
        f
      }
    }
    def visitLeave(dep: DependencyNode): Boolean = {
      require(stack.head == dep.getDependency, s"stack mismatch: ${stack.head} != ${dep.getDependency}")
      stack = stack.tail
      true
    }
  }
}
