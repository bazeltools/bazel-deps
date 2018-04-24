package com.github.johnynek.bazel_deps


import org.scalatest.FunSuite

class CoursierTest extends FunSuite  {
  test("test bouncycastle example") {
    val config = """
options:
  buildHeader: [ "load(\"@io_bazel_rules_scala//scala:scala_import.bzl\", \"scala_import\")" ]
  languages: [ "java", "scala:2.11.8" ]
  resolverType: "coursier"
  resolvers:
    - id: "mavencentral"
      type: "default"
      url: https://repo.maven.apache.org/maven2/
  transitivity: runtime_deps
  versionConflictPolicy: highest

dependencies:
  com.lowagie:
    itext:
      lang: java
      version: "2.1.7"
      exclude:
        - "bouncycastle:bcprov-jdk14"
        - "bouncycastle:bcmail-jdk14"
        - "bouncycastle:bctsp-jdk14"
  org.xhtmlrenderer:
    flying-saucer-pdf:
      lang: java
      version: "9.0.3"
"""

    val model = Decoders.decodeModel(Yaml, config).right.get
    val (graph, shas, normalized) = MakeDeps.runResolve(model, null).get
    normalized.keys.foreach { uvc =>
      assert(!uvc.asString.contains("bouncycastle"))
    }
  }
}
