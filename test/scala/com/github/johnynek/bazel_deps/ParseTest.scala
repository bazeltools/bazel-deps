package com.github.johnynek.bazel_deps

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks._
import ParseTestUtil._

class ParseTest extends FunSuite {
  test("parse a file with no options, yaml") {
    val str = """dependencies:
                |  com.twitter:
                |    scalding:
                |      lang: scala
                |      version: "0.16.0"
                |      modules: [core, args, date]
                |""".stripMargin('|')
                // |
                // |options:
                // |  languages: ["scala:2.11.8", java]
                // |  thirdPartyDirectory: 3rdparty/jvm

    assert(Decoders.decodeModel(Yaml, str) ==
      Right(Model(
        Dependencies(
          MavenGroup("com.twitter") ->
            Map(ArtifactOrProject("scalding") ->
              ProjectRecord(
                Language.Scala.default,
                Some(Version("0.16.0")),
                Some(Set("core", "args", "date").map(Subproject(_))),
                None,
                None,
                None,
                None,
                None))),
          None,
          None)))
  }
  test("parse a file with options, yaml") {
    val str = """dependencies:
                |  com.twitter:
                |    scalding:
                |      lang: scala
                |      version: "0.16.0"
                |      modules: [core, args, date]
                |
                |options:
                |  languages: ["scala:2.11.7", java]
                |  thirdPartyDirectory: 3rdparty/jvm
                |  resolverCache: bazel_output_base
                |  licenses: ["unencumbered", "permissive"]
                |  buildFileName: "BUILD.bazel"
                |""".stripMargin('|')

    assert(Decoders.decodeModel(Yaml, str) ==
      Right(Model(
        Dependencies(
          MavenGroup("com.twitter") ->
            Map(ArtifactOrProject("scalding") ->
              ProjectRecord(
                Language.Scala(Version("2.11.7"), true),
                Some(Version("0.16.0")),
                Some(Set("core", "args", "date").map(Subproject(_))),
                None,
                None,
                None,
                None,
                None))),
          None,
          Some(
            Options(
              None,
              Some(DirectoryName("3rdparty/jvm")),
              Some(Set(Language.Scala(Version("2.11.7"), true), Language.Java)),
              None,
              None,
              None,
              Some(ResolverCache.BazelOutputBase),
              None,
              Some(Set("unencumbered", "permissive")),
              None,
              None,
              Some("BUILD.bazel"))))))
  }
  test("parse empty subproject version") {
    val str = """dependencies:
                |  com.twitter:
                |    scalding:
                |      lang: scala
                |      version: 0.16.0
                |      modules: ["", core, args, date]
                |
                |options:
                |  languages: ["scala:2.11.7", java]
                |  thirdPartyDirectory: 3rdparty/jvm
                |""".stripMargin('|')

    assert(Decoders.decodeModel(Yaml, str) ==
      Right(Model(
        Dependencies(
          MavenGroup("com.twitter") ->
            Map(ArtifactOrProject("scalding") ->
              ProjectRecord(
                Language.Scala(Version("2.11.7"), true),
                Some(Version("0.16.0")),
                Some(Set("", "core", "args", "date").map(Subproject(_))),
                None,
                None,
                None,
                None,
                None))),
          None,
          Some(
            Options(
              None,
              Some(DirectoryName("3rdparty/jvm")),
              Some(Set(Language.Scala(Version("2.11.7"), true), Language.Java)),
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None,
              None)))))

    assert(MavenArtifactId(ArtifactOrProject("a"), Subproject("")).asString === "a")
    assert(MavenArtifactId(ArtifactOrProject("a"), Subproject("b")).asString === "a-b")
  }


  test("parse a file with an annotationProcessor defined") {
    val str = """dependencies:
                |  com.google.auto.value:
                |    auto-value:
                |      version: "1.5"
                |      lang: java
                |      processorClasses: ["com.google.auto.value.processor.AutoValueProcessor"]
                |""".stripMargin('|')

    assert(Decoders.decodeModel(Yaml, str) ==
      Right(Model(
        Dependencies(
          MavenGroup("com.google.auto.value") ->
            Map(ArtifactOrProject("auto-value") ->
              ProjectRecord(
                Language.Java,
                Some(Version("1.5")),
                None,
                None,
                None,
                None,
                Some(Set(ProcessorClass("com.google.auto.value.processor.AutoValueProcessor"))),
                None))),
        None,
        None)))
  }

  test("parse a file with an annotationProcessor defined and generatesApi false") {
    val str = """dependencies:
                |  com.google.auto.value:
                |    auto-value:
                |      version: "1.5"
                |      lang: java
                |      generatesApi: false
                |      processorClasses: ["com.google.auto.value.processor.AutoValueProcessor"]
                |""".stripMargin('|')

    assert(Decoders.decodeModel(Yaml, str) ==
      Right(Model(
        Dependencies(
          MavenGroup("com.google.auto.value") ->
            Map(ArtifactOrProject("auto-value") ->
              ProjectRecord(
                Language.Java,
                Some(Version("1.5")),
                None,
                None,
                None,
                Some(false),
                Some(Set(ProcessorClass("com.google.auto.value.processor.AutoValueProcessor"))),
                None))),
        None,
        None)))
  }

  test("parse a file with an annotationProcessor defined and generatesApi true") {
    val str = """dependencies:
                |  com.google.auto.value:
                |    auto-value:
                |      version: "1.5"
                |      lang: java
                |      generatesApi: true
                |      processorClasses: ["com.google.auto.value.processor.AutoValueProcessor"]
                |""".stripMargin('|')

    assert(Decoders.decodeModel(Yaml, str) ==
      Right(Model(
        Dependencies(
          MavenGroup("com.google.auto.value") ->
            Map(ArtifactOrProject("auto-value") ->
              ProjectRecord(
                Language.Java,
                Some(Version("1.5")),
                None,
                None,
                None,
                Some(true),
                Some(Set(ProcessorClass("com.google.auto.value.processor.AutoValueProcessor"))),
                None))),
        None,
        None)))
  }

  test("parse a file that includes packaging for an artifact") {
    val str = """dependencies:
                |  com.google.auto.value:
                |    auto-value:dll:
                |      version: "1.5"
                |      lang: java
                |""".stripMargin('|')

    assert(Decoders.decodeModel(Yaml, str) ==
      Right(Model(
        Dependencies(
          MavenGroup("com.google.auto.value") ->
            Map(ArtifactOrProject("auto-value:dll") ->
              ProjectRecord(
                Language.Java,
                Some(Version("1.5")),
                None,
                None,
                None,
                None,
                None,
                None))),
        None,
        None)))
  }

  test("parse a file that includes packaging for an artifact with subprojects") {
    val str = """dependencies:
                |  com.google.auto.value:
                |    auto-value:dll:
                |      modules: ["", "extras"]
                |      version: "1.5"
                |      lang: java
                |""".stripMargin('|')

    assert(Decoders.decodeModel(Yaml, str) ==
      Right(Model(
        Dependencies(
          MavenGroup("com.google.auto.value") ->
            Map(ArtifactOrProject("auto-value:dll") ->
              ProjectRecord(
                Language.Java,
                Some(Version("1.5")),
                Some(Set("", "extras").map(Subproject(_))),
                None,
                None,
                None,
                None,
                None))),
        None,
        None)))
  }

  test("parse a file that includes classifier") {
    val str = """dependencies:
                |  com.google.auto.value:
                |    auto-value:dll:best-one:
                |      version: "1.5"
                |      lang: java
                |""".stripMargin('|')

    assert(Decoders.decodeModel(Yaml, str) ==
      Right(Model(
        Dependencies(
          MavenGroup("com.google.auto.value") ->
            Map(ArtifactOrProject("auto-value:dll:best-one") ->
              ProjectRecord(
                Language.Java,
                Some(Version("1.5")),
                None,
                None,
                None,
                None,
                None,
                None))),
        None,
        None)))
  }

  test("parse a file that _excludes_ something with a classifier") {
    val str = """dependencies:
                |  com.google.auto.value:
                |    auto-value:
                |      exclude:
                |        - "foo:bar:so:fancy"
                |      version: "1.5"
                |      lang: java
                |""".stripMargin('|')

    assert(Decoders.decodeModel(Yaml, str) ==
      Right(Model(
        Dependencies(
          MavenGroup("com.google.auto.value") ->
            Map(ArtifactOrProject("auto-value") ->
              ProjectRecord(
                Language.Java,
                Some(Version("1.5")),
                None,
                None,
                Some(Set((MavenGroup("foo"), ArtifactOrProject(MavenArtifactId("bar:so:fancy"))))),
                None,
                None,
                None))),
        None,
        None)))
  }

  test("parse a file that has generateNeverlink set to true") {
    val str = """dependencies:
                |  org.apache.tomcat:
                |    tomcat-catalina:
                |      version: "7.0.57"
                |      lang: java
                |      generateNeverlink: true
                |""".stripMargin('|')

    assert(Decoders.decodeModel(Yaml, str) ==
      Right(Model(
        Dependencies(
          MavenGroup("org.apache.tomcat") ->
            Map(ArtifactOrProject("tomcat-catalina") ->
              ProjectRecord(
                Language.Java,
                Some(Version("7.0.57")),
                None,
                None,
                None,
                None,
                None,
                Some(true)))),
        None,
        None)))
  }

  test("parse a file that has generateNeverlink set to false") {
    val str = """dependencies:
                |  org.apache.tomcat:
                |    tomcat-catalina:
                |      version: "7.0.57"
                |      lang: java
                |      generateNeverlink: false
                |""".stripMargin('|')

    assert(Decoders.decodeModel(Yaml, str) ==
      Right(Model(
        Dependencies(
          MavenGroup("org.apache.tomcat") ->
            Map(ArtifactOrProject("tomcat-catalina") ->
              ProjectRecord(
                Language.Java,
                Some(Version("7.0.57")),
                None,
                None,
                None,
                None,
                None,
                Some(false)))),
        None,
        None)))
  }

  /*
   * TODO make this test pass
   * see: https://github.com/johnynek/bazel-deps/issues/15
  test("parse fails with duplicated groups") {
    val str = """dependencies:
                |  com.twitter:
                |    scalding:
                |      lang: scala
                |      version: 0.16.0
                |      modules: ["", core, args, date]
                |  com.twitter:
                |    foo:
                |      lang: java
                |      version: "42"
                |
                |""".stripMargin('|')

    assert(Decoders.decodeModel(Yaml, str).isLeft)
  }
*/

}
