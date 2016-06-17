package com.github.johnynek.bazel_deps

object MakeDeps {
  def main(args: Array[String]): Unit = {
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
        println(Writer.workspace(g))
        println("########")
        Writer.targets(g).groupBy(_.name.path).foreach { case (p, items) =>
          println(p)
          println("")
          items.foreach { t => println(t.toBazelString + "\n") }
        }
    }
  }
}
