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
              Some(Set("unencumbered", "permissive")))))))
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
                Some(Set(ProcessorClass("com.google.auto.value.processor.AutoValueProcessor")))))),
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

  test("parse classifiers") {
    val str = """dependencies:
                |  com.github.jnr:
                |    jffi:
                |      lang: java
                |      version: 1.2.16
                |      classifiers: ["", javadoc, native]
                |""".stripMargin('|')

    assert(Decoders.decodeModel(Yaml, str) ==
      Right(Model(
        Dependencies(
          MavenGroup("com.github.jnr") ->
            Map(ArtifactOrProject("jffi") ->
              ProjectRecord(
                Language.Java,
                Some(Version("1.2.16")),
                None,
                Some(Set("", "javadoc", "native").map(Classifier(_))),
                None,
                None,
                None))),
          None,
          None)))

    assert(MavenArtifactId(ArtifactOrProject("a"), Classifier("")).asString === "a")
    assert(MavenArtifactId(ArtifactOrProject("a"), Classifier("c")).asString === "a:jar:c")
    assert(MavenArtifactId("a").getClassifier === None)
    assert(MavenArtifactId("a:jar:c").getClassifier === Some("c"))
  }

}
