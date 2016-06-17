package com.github.johnynek.bazel_deps

object Writer {
  def workspace(g: Graph[MavenCoordinate, Unit]): String = {
    val nodes = g.nodes
    val lines = nodes.map { case coord@MavenCoordinate(g, a, v) =>
        s"""  callback({"name": "${coord.toBazelRepoName}", "artifact": "${coord.asString}"})"""
      }
      .mkString("\n")
    s"""def maven_dependencies(callback):\n$lines"""
  }
  /*
load("//3rdparty:workspace.bzl", "maven_dependencies")

maven_dependencies(maven_jar)
*/
}
