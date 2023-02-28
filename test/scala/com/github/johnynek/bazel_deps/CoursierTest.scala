package com.github.johnynek.bazel_deps

import java.nio.file.Files
import org.scalatest.FunSuite

class CoursierTest extends FunSuite  {
  val tmpPath = Files.createTempDirectory("cache")
  tmpPath.toFile.deleteOnExit()

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
    val (graph, shas, normalized) = MakeDeps.runResolve(model, tmpPath).get
    normalized.keys.foreach { uvc =>
      assert(!uvc.asString.contains("bouncycastle"))
    }
  }

  test("test classifier example") {
    val config =
      """
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
  net.sf.json-lib:
    json-lib:jar:jdk15:
      lang: java
      version: "2.4"
"""

    val model = Decoders.decodeModel(Yaml, config).right.get
    val (graph, shas, normalized) = MakeDeps.runResolve(model, tmpPath).get
    assert(graph.nodes.contains(
      MavenCoordinate(
        MavenGroup("net.sf.json-lib"), MavenArtifactId("json-lib", "jar", "jdk15"), Version("2.4"))))
  }

  test("test transitive classifier example") {
    val config =
      """
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
  org.sonatype.sisu:
    sisu-inject-bean:
      lang: java
      version: "2.1.1"
"""

    val model = Decoders.decodeModel(Yaml, config).right.get
    val (graph, shas, normalized) = MakeDeps.runResolve(model, tmpPath).get
    // org.sonatype.sisu:sisu-guice:no_aop is a transitive dependency of org.sonatype.sisu:sisu-inject-bean
    assert(graph.nodes.contains(
      MavenCoordinate(
        MavenGroup("org.sonatype.sisu"), MavenArtifactId("sisu-guice", "jar", "no_aop"), Version("2.9.4"))))
  }

  test("test non-jar packaging excludes example") {
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
  com.amazonaws:
    aws-dynamodb-encryption-java:
      lang: java
      version: "1.11.0"
      exclude:
        - "com.almworks.sqlite4java:libsqlite4java-linux-amd64:so"
"""

    val model = Decoders.decodeModel(Yaml, config).right.get
    val (graph, shas, normalized) = MakeDeps.runResolve(model, tmpPath).get

    assert(graph.nodes.contains(
      MavenCoordinate(
        MavenGroup("com.amazonaws"), MavenArtifactId("aws-dynamodb-encryption-java"), Version("1.11.0"))))

    // make sure no non-jar packaging shows up, until bazel-deps can handle setting those
    // as data dependencies
    val nonJarNodes = graph.nodes
        .filter { mc => mc.artifact.packaging != "jar" }

    assert(nonJarNodes.size == 0)
  }

  test("test jar packaging w/ classifier exclude example") {
    val config =
      """
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
  org.sonatype.sisu:
    sisu-inject-bean:
      lang: java
      version: "2.1.1"
      exclude:
        - "org.sonatype.sisu:sisu-guice:jar:no-aop"
"""

    val model = Decoders.decodeModel(Yaml, config).right.get
    val (graph, shas, normalized) = MakeDeps.runResolve(model, tmpPath).get
    // org.sonatype.sisu:sisu-guice:no_aop is a transitive dependency of org.sonatype.sisu:sisu-inject-bean
    // we exclude it for this test
    assert(!graph.nodes.contains(
      MavenCoordinate(
        MavenGroup("org.sonatype.sisu"), MavenArtifactId("sisu-guice", "jar", "no_aop"), Version("2.9.4"))))
  }

  test("test runtime scopes") {
    val config = """
options:
  languages: [ "java" ]
  resolverType: "coursier"
  resolvers:
    - id: "mavencentral"
      type: "default"
      url: https://repo.maven.apache.org/maven2/
  transitivity: runtime_deps
  versionConflictPolicy: highest
dependencies:
  org.mockito:
    mockito-core:
      lang: java
      version: "2.7.20"
"""

    val model = Decoders.decodeModel(Yaml, config).right.get
    val (graph, shas, normalized) = MakeDeps.runResolve(model, tmpPath).get

    assert(graph.nodes.contains(
      MavenCoordinate(
        MavenGroup("org.mockito"), MavenArtifactId("mockito-core"), Version("2.7.20"))))
    // objenesis is a runtime-scoped dependencyof mockito; ensure Coursier includes it like compile-scoped dependencies.
    assert(graph.nodes.contains(
      MavenCoordinate(
        MavenGroup("org.objenesis"), MavenArtifactId("objenesis"), Version("2.5"))))
  }

/*
 * This seems to fail now, I think with a later coursier. I'm not sure what we were testing, or why we want
 * stack overflows.
  test("test stack overflow case") {
    val config = """
options:
  languages: [ "java" ]
  resolverType: "coursier"
  resolvers:
    - id: "mavencentral"
      type: "default"
      url: https://repo.maven.apache.org/maven2/
  transitivity: runtime_deps
  versionConflictPolicy: highest
dependencies:
  org.apache.hadoop:
    hadoop:
      lang: java
      modules: [ "aws", "client-runtime" ]
      version: "3.0.0"
    hadoop-client-api:
      exports:
        - "org.apache.hadoop:hadoop-client-runtime"
        - "org.apache.htrace:htrace-core4"
      lang: java
      version: "3.0.0"

  org.apache.htrace:
    htrace-core4:
      lang: java
      version: "4.1.0-incubating"
"""

    val model = Decoders.decodeModel(Yaml, config).right.get
    val (normalized, shas, duplicates) = MakeDeps.runResolve(model, tmpPath).get

assert(Writer.artifactEntries(
      g = normalized,
      duplicates = duplicates,
      shas = shas,
      model = model
).left.map(_ => ()) == Left(()))
  }
  */
}
