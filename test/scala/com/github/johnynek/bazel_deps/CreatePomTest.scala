package com.github.johnynek.bazel_deps

import org.scalatest.FunSuite

import java.nio.file.Files

class CreatePomTest extends FunSuite{

  test("CreatePom translates dependencies yaml file into pom xml file") {
    val dependenciesYaml = """
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

    val expectedPomXml =
      <project>
        <modelVersion>4.0.0</modelVersion>
        <dependencies>
          <dependency>
            <groupId>com.lowagie</groupId>
            <artifactId>itext</artifactId>
            <version>2.1.7</version>
          </dependency>
          <dependency>
            <groupId>org.xhtmlrenderer</groupId>
            <artifactId>flying-saucer-pdf</artifactId>
            <version>9.0.3</version>
          </dependency>
          <dependency>
            <groupId>org.xhtmlrenderer</groupId>
            <artifactId>flying-saucer-core</artifactId>
            <version>9.0.3</version>
          </dependency>
        </dependencies>
      </project>

      val model = Decoders.decodeModel(Yaml, dependenciesYaml).right.get
      val tmpPath = Files.createTempDirectory("cache")
      tmpPath.toFile.deleteOnExit()
      val (dependencies, _, _) = MakeDeps.runResolve(model, tmpPath).get
      val p = new scala.xml.PrettyPrinter(80, 2)

      assert(CreatePom.translate(dependencies) == p.format(expectedPomXml))
  }
}
