package com.github.johnynek.bazel_deps

import java.io.{File, FileOutputStream}

object MakeDeps {
  def main(args: Array[String]): Unit = {
    val workspacePath = args(0)
    val projectRoot = args(1)
    val thirdParty = args(2)

    val resolver = new Resolver(List(MavenServer("central", "default", "http://central.maven.org/maven2/")))
    val aether = "org.eclipse.aether"
    val aetherVersion = "1.0.2.v20150114"
    val aethers = List(
      "aether-api",
      "aether-impl",
      "aether-connector-basic",
      "aether-transport-file",
      "aether-transport-http").map { art => s"$aether:$art:$aetherVersion" }

    val allDeps = ("org.apache.maven:maven-aether-provider:3.1.0" ::
      "com.google.guava:guava:18.0" ::
      "args4j:args4j:2.32" ::
      aethers).map(MavenCoordinate(_))

    val graph = resolver.addAll(Graph.empty, allDeps)
    allDeps.foreach { m => require(graph.nodes(m), s"$m") }
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
        val targets = Writer.targets(g, toPath(thirdParty))
        Writer.createBuildFiles(new File(projectRoot), targets)
        println(s"wrote ${targets.size} targets")
    }
  }
}
