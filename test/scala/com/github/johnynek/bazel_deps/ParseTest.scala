package com.github.johnynek.bazel_deps

import cats.data.Xor
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks._

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
      Xor.right(Model(
        Dependencies(
          MavenGroup("com.twitter") ->
            Map(ArtifactOrProject("scalding") ->
              ProjectRecord(
                Language.Scala(Version("2.11.8"), true),
                Some(Version("0.16.0")),
                Some(Set("core", "args", "date").map(Subproject(_))),
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
                |""".stripMargin('|')

    assert(Decoders.decodeModel(Yaml, str) ==
      Xor.right(Model(
        Dependencies(
          MavenGroup("com.twitter") ->
            Map(ArtifactOrProject("scalding") ->
              ProjectRecord(
                Language.Scala(Version("2.11.7"), true),
                Some(Version("0.16.0")),
                Some(Set("core", "args", "date").map(Subproject(_))),
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
              None)))))
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
      Xor.right(Model(
        Dependencies(
          MavenGroup("com.twitter") ->
            Map(ArtifactOrProject("scalding") ->
              ProjectRecord(
                Language.Scala(Version("2.11.7"), true),
                Some(Version("0.16.0")),
                Some(Set("", "core", "args", "date").map(Subproject(_))),
                None,
                None
                ))),
          None,
          Some(
            Options(
              None,
              Some(DirectoryName("3rdparty/jvm")),
              Some(Set(Language.Scala(Version("2.11.7"), true), Language.Java)),
              None,
              None,
              None)))))

    assert(MavenArtifactId(ArtifactOrProject("a"), Subproject("")).asString === "a")
    assert(MavenArtifactId(ArtifactOrProject("a"), Subproject("b")).asString === "a-b")
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

  test("parse randomly generated Model.toDoc") {
    // this test is slow and takes a lot of memory sadly
    implicit val generatorDrivenConfig =
      PropertyCheckConfig(minSuccessful = 20)

    forAll(ModelGenerators.modelGen) { model =>
      val str = model.toDoc.render(70)
      val good = Decoders.decodeModel(Yaml, str) == Xor.right(model)
      if (!good) {
        println(model)
        println(str)
      }
      assert(Decoders.decodeModel(Yaml, str) == Xor.right(model))
    }
  }
}
