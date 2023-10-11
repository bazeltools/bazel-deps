package com.github.johnynek.bazel_deps

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{ Files, Paths }

class CreatePomTest extends AnyFunSuite {

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
    - id: "mavenother"
      type: "default"
      url: https://repo1.maven.apache.org/maven2/
  transitivity: runtime_deps
  versionConflictPolicy: highest
  strictVisibility: true
 

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
  io.netty:
    netty-resolver-dns-native-macos:jar:osx-x86_64:
      lang: java
      version: "4.1.85.Final"
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
            <groupId>io.netty</groupId>
            <artifactId>netty-buffer</artifactId>
            <version>4.1.85.Final</version>
          </dependency>
          <dependency>
            <groupId>io.netty</groupId>
            <artifactId>netty-codec</artifactId>
            <version>4.1.85.Final</version>
          </dependency>
          <dependency>
            <groupId>io.netty</groupId>
            <artifactId>netty-codec-dns</artifactId>
            <version>4.1.85.Final</version>
          </dependency>
          <dependency>
            <groupId>io.netty</groupId>
            <artifactId>netty-common</artifactId>
            <version>4.1.85.Final</version>
          </dependency>
          <dependency>
            <groupId>io.netty</groupId>
            <artifactId>netty-handler</artifactId>
            <version>4.1.85.Final</version>
          </dependency>
          <dependency>
            <groupId>io.netty</groupId>
            <artifactId>netty-resolver</artifactId>
            <version>4.1.85.Final</version>
          </dependency>
          <dependency>
            <groupId>io.netty</groupId>
            <artifactId>netty-resolver-dns</artifactId>
            <version>4.1.85.Final</version>
          </dependency>
          <dependency>
            <groupId>io.netty</groupId>
            <artifactId>netty-resolver-dns-classes-macos</artifactId>
            <version>4.1.85.Final</version>
          </dependency>
          <dependency>
            <groupId>io.netty</groupId>
            <artifactId>netty-resolver-dns-native-macos</artifactId>
            <classifier>osx-x86_64</classifier>
            <version>4.1.85.Final</version>
          </dependency>
          <dependency>
            <groupId>io.netty</groupId>
            <artifactId>netty-transport</artifactId>
            <version>4.1.85.Final</version>
          </dependency>
          <dependency>
            <groupId>io.netty</groupId>
            <artifactId>netty-transport-native-unix-common</artifactId>
            <version>4.1.85.Final</version>
          </dependency>
          <dependency>
            <groupId>org.xhtmlrenderer</groupId>
            <artifactId>flying-saucer-core</artifactId>
            <version>9.0.3</version>
          </dependency>
          <dependency>
            <groupId>org.xhtmlrenderer</groupId>
            <artifactId>flying-saucer-pdf</artifactId>
            <version>9.0.3</version>
          </dependency>
        </dependencies>
      </project>

      val model = Decoders.decodeModel(Yaml, dependenciesYaml).right.get
      val tmpPath = Files.createTempDirectory("cache")
      tmpPath.toFile.deleteOnExit()
      val (dependencies, _, _) = MakeDeps.runResolve(Paths.get("/"), model, tmpPath).get
      val p = new scala.xml.PrettyPrinter(80, 2)
    assert(CreatePom.translate(dependencies) == p.format(expectedPomXml))
  }
}
