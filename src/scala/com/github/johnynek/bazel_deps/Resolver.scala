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

  type Node = (MavenCoordinate, Explicitness)

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

    def visitEnter(depNode: DependencyNode): Boolean = {
      val dep = depNode.getDependency
      if (visited(dep)) false
      else {
        visited = visited + dep
        stack match {
          case Nil => ()
          case h :: Nil =>
            // this is an explicit dependency
            currentDeps = currentDeps
              .addEdge(Edge((coord(h), Explicitness.Explicit), (coord(dep), Explicitness.Implicit), ()))
          case h :: _ =>
            // this is an implicit dependency
            currentDeps = currentDeps
              .addEdge(Edge((coord(h), Explicitness.Implicit), (coord(dep), Explicitness.Implicit), ()))
        }
        stack = dep :: stack
        true
      }
    }
    def visitLeave(dep: DependencyNode): Boolean = {
      require(stack.head == dep.getDependency, s"stack mismatch: ${stack.head} != ${dep.getDependency}")
      stack = stack.tail
      true
    }
  }
    /*
    PreorderNodeListGenerator visitor = new PreorderNodeListGenerator();
    collectResult.getRoot().accept(visitor);

    return ImmutableSet.copyOf(
      visitor.getNodes().stream()
        .filter(d -> !d.getDependency().isOptional())
        .map(DependencyNode::getArtifact)
        .collect(Collectors.toList()));
  }
*/
}

/*
import org.apache.maven.repository.internal.MavenRepositorySystemUtils;
import org.eclipse.aether.DefaultRepositorySystemSession;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.artifact.Artifact;
import org.eclipse.aether.collection.CollectRequest;
import org.eclipse.aether.collection.CollectResult;
import org.eclipse.aether.collection.DependencyCollectionException;
import org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.graph.DependencyNode;
import org.eclipse.aether.impl.DefaultServiceLocator;
import org.eclipse.aether.repository.LocalRepository;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.spi.connector.RepositoryConnectorFactory;
import org.eclipse.aether.spi.connector.transport.TransporterFactory;
import org.eclipse.aether.transport.file.FileTransporterFactory;
import org.eclipse.aether.transport.http.HttpTransporterFactory;
import org.eclipse.aether.util.graph.visitor.PreorderNodeListGenerator;

public class Maven {

  public static Set<Artifact> transitiveDependencies(Artifact artifact) {

    RepositorySystem system = newRepositorySystem();

    RepositorySystemSession session = newRepositorySystemSession(system);

    CollectRequest collectRequest = new CollectRequest();
    collectRequest.setRoot(new Dependency(artifact, ""));
    collectRequest.setRepositories(repositories());

    CollectResult collectResult = null;
    try {
      collectResult = system.collectDependencies(session, collectRequest);
    } catch (DependencyCollectionException e) {
      throw new RuntimeException(e);
    }

    PreorderNodeListGenerator visitor = new PreorderNodeListGenerator();
    collectResult.getRoot().accept(visitor);

    return ImmutableSet.copyOf(
      visitor.getNodes().stream()
        .filter(d -> !d.getDependency().isOptional())
        .map(DependencyNode::getArtifact)
        .collect(Collectors.toList()));
  }

  private static RepositorySystem newRepositorySystem() {
    DefaultServiceLocator locator = MavenRepositorySystemUtils.newServiceLocator();
    locator.addService(RepositoryConnectorFactory.class, BasicRepositoryConnectorFactory.class);
    locator.addService(TransporterFactory.class, FileTransporterFactory.class);
    locator.addService(TransporterFactory.class, HttpTransporterFactory.class);

    locator.setErrorHandler(new DefaultServiceLocator.ErrorHandler() {
      @Override
      public void serviceCreationFailed(Class<?> type, Class<?> impl, Throwable exception) {
        exception.printStackTrace();
      }
    });

    return locator.getService(RepositorySystem.class);
  }

  public static DefaultRepositorySystemSession newRepositorySystemSession(RepositorySystem system) {
    DefaultRepositorySystemSession session = MavenRepositorySystemUtils.newSession();

    LocalRepository localRepo = new LocalRepository("target/local-repo");
    session.setLocalRepositoryManager(system.newLocalRepositoryManager(session, localRepo));

    return session;
  }

  public static List<RemoteRepository> repositories() {
    return ImmutableList.of(
      new RemoteRepository.Builder("central", "default", "http://central.maven.org/maven2/")
        .build());
  }
}
*/
