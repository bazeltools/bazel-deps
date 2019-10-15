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
    val (graph, shas, normalized) = MakeDeps.runResolve(model, null).get
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
    val (graph, shas, normalized) = MakeDeps.runResolve(model, null).get
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
    val (graph, shas, normalized) = MakeDeps.runResolve(model, null).get

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
    val (graph, shas, normalized) = MakeDeps.runResolve(model, null).get
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
    val (graph, shas, normalized) = MakeDeps.runResolve(model, null).get

    assert(graph.nodes.contains(
      MavenCoordinate(
        MavenGroup("org.mockito"), MavenArtifactId("mockito-core"), Version("2.7.20"))))
    // objenesis is a runtime-scoped dependencyof mockito; ensure Coursier includes it like compile-scoped dependencies.
    assert(graph.nodes.contains(
      MavenCoordinate(
        MavenGroup("org.objenesis"), MavenArtifactId("objenesis"), Version("2.5"))))
  }

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
    val (normalized, shas, duplicates) = MakeDeps.runResolve(model, null).get

    assert(Writer.targets(normalized, model).isLeft)
  }

  test("version alignment breaks resolution") {
    def configForVersions(firstVersion: String, secondVersion: String) = s"""
options:
  buildHeader: [ "load(\\"@io_bazel_rules_scala//scala:scala_import.bzl\\", \\"scala_import\\")" ]
  languages: [ "java", "scala:2.11.8" ]
  resolverType: "coursier"
  resolvers:
    - id: "mavencentral"
      type: "default"
      url: https://repo.maven.apache.org/maven2/
  transitivity: runtime_deps
  versionConflictPolicy: highest

dependencies:
  org.kie:
    kie-api:
      lang: java
      version: "${firstVersion}"

  org.kie.server:
    kie:
      lang: java
      modules: [ "server-api", "server-client" ]
      version: "${secondVersion}"
"""

    def testConfig(firstVersion: String, secondVersion: String): Unit = {
      val model = Decoders.decodeModel(Yaml, configForVersions(firstVersion, secondVersion)).right.get
      val (normalized, shas, duplicates) = MakeDeps.runResolve(model, null).get
      val written = Writer.targets(normalized, model)

      written match {
        case Left(errors) =>
            assert(false, errors)
          case Right(_) => ()
      }
    }

    testConfig("7.26.0.Final", "7.27.0.Final")
    testConfig("7.27.0.Final", "7.26.0.Final")
    testConfig("7.27.0.Final", "7.27.0.Final")
  }
}
