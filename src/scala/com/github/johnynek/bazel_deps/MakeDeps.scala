package com.github.johnynek.bazel_deps

import java.io.{File, FileOutputStream}

object MakeDeps {
  def main(args: Array[String]): Unit = {
    val workspacePath = args(0)
    val projectRoot = args(1)
    val thirdParty = args(2)

    val resolver = new Resolver(List(MavenServer("central", "default", "http://central.maven.org/maven2/")))

    def java(dep: String) =
      dep.split(':') match {
        case Array(g, a, v) =>
          MavenGroup(g) ->
            Map(ArtifactOrProject(a) ->
              ProjectRecord(
                Language.Java,
                Version(v),
                Nil))
        case _ => sys.error(s"expect two colons, got: $dep")
      }

    def scala(dep: String) =
      dep.split(':') match {
        case Array(g, a, v) =>
          MavenGroup(g) ->
            Map(ArtifactOrProject(a) ->
              ProjectRecord(
                Language.Scala(Version("2.11")),
                Version(v),
                Nil))
        case _ => sys.error(s"expect two colons, got: $dep")
      }

    val deps = Dependencies(Map(
      MavenGroup("org.eclipse.aether") ->
        Map(ArtifactOrProject("aether") ->
          ProjectRecord(
            Language.Java,
            Version("1.0.2.v20150114"),
            List("api", "impl", "connector-basic", "transport-file", "transport-http").map(Subproject(_)))),

      java("org.apache.maven:maven-aether-provider:3.1.0"),
      scala("org.scalacheck:scalacheck:1.12.0")
      ))

    val graph = resolver.addAll(Graph.empty, deps.roots)
    deps.roots.foreach { m => require(graph.nodes(m), s"$m") }
    Normalizer(graph, Options(Some(VersionConflictPolicy.Highest), None, None)) match {
      case None =>
        println("[ERROR] could not normalize versions:")
        println(graph.nodes.groupBy(_.unversioned)
          .mapValues { _.map(_.version).toList.sorted }
          .filter { case (_, s) => s.lengthCompare(1) > 0 }
          .map { case (u, vs) => s"""${u.asString}: ${vs.mkString(", ")}\n""" }
          .mkString("\n"))
        System.exit(1)
      case Some(g) =>
        //println(g.show(_.asString))

        val ws = new FileOutputStream(new File(workspacePath))
        ws.write(Writer.workspace(g).getBytes("UTF-8"))
        def toPath(str: String): List[String] = str.split('/').filter(_.nonEmpty).toList
        println(toPath(thirdParty))
        val targets = Writer.targets(g, toPath(thirdParty), Model(deps, None, None))
        Writer.createBuildFiles(new File(projectRoot), targets)
        println(s"wrote ${targets.size} targets")
    }
  }
}
