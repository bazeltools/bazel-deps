package com.github.johnynek.bazel_deps

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
      Right(Model(
        Dependencies(
          MavenGroup("com.twitter") ->
            Map(ArtifactOrProject("scalding") ->
              ProjectRecord(
                Language.Scala.default,
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
                |  resolverCache: bazel_output_base
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
              Some(ResolverCache.BazelOutputBase))))))
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

  def decode(str: String): Model = {
    val Right(mod) = Decoders.decodeModel(Yaml, str)
    mod
  }

  def law(model: Model) = {
    val str = model.toDoc.render(70)
    val decoded = decode(str)
    // if (decoded != model) {
    //   println(str)
    //   println("------")
    //   println(decoded.toDoc.render(70))
    // }
    assert(decoded == model || decoded.flatten === model.flatten)
    assert(decoded.toDoc.render(70) === str)
  }

  test("parse randomly generated Model.toDoc") {
    // this test is slow and takes a lot of memory sadly
    implicit val generatorDrivenConfig =
      PropertyCheckConfig(minSuccessful = 200)

    forAll(ModelGenerators.modelGen)(law _)
  }

  test("test a regression") {
    import Language.{Java, Scala}

    // This has a single sub-project, which we don't minimize into this form
    val model = Model(Dependencies(
      Map(
        MavenGroup("n2rr") ->
          Map(
            ArtifactOrProject("zmup") -> ProjectRecord(Java,Some(Version("019")),Some(Set(Subproject("wcv"))),Some(Set((MavenGroup("j9szw4"),ArtifactOrProject("i")))),None)
          )
        )),Some(Replacements(Map())),None)

    law(model)
  }

  def roundTripsTo(input: String, output: String) = {
    val mod = decode(input)
    val modStr = mod.toDoc.render(70)
    //assert(decode(modStr) === mod)
    assert(modStr === output)
  }

  test("don't combine incorrectly") {
    val str = """dependencies:
                |  com.twitter:
                |    scalding:
                |      lang: scala
                |      modules: [ "", "args", "core", "date" ]
                |      version: "0.16.0"
                |    scalding-bar:
                |      lang: java
                |    scalding-foo:
                |      lang: java
                |""".stripMargin('|')

    roundTripsTo(str, str)
  }

  test("example regression: util") {
    val input = """
dependencies:
  com.twitter:
    util:
      lang: scala
      modules: [ "codec", "core", "stats" ]
      version: "6.26.0"
    util-cache:
      lang: scala
      version: "6.29.0"
    util-collection:
      lang: scala
      version: "6.29.0"
    util-events:
      lang: scala
      version: "6.29.0"
"""
    // we have two clusters of equal size
    // merge the one with the lowest version
    val output = """dependencies:
  com.twitter:
    util:
      lang: scala
      modules: [ "codec", "core", "stats" ]
      version: "6.26.0"
    util-cache:
      lang: scala
      version: "6.29.0"
    util-collection:
      lang: scala
      version: "6.29.0"
    util-events:
      lang: scala
      version: "6.29.0"
"""
    roundTripsTo(input, output)
  }

  test("maximize the project name") {
    val input = """
dependencies:
   com.amazonaws:
     aws:
       lang: java
       modules: [ "java-sdk-core", "java-sdk-ec2", "java-sdk-s3" ]
       version: "1.11.5"
"""
    val output = """dependencies:
  com.amazonaws:
    aws-java-sdk:
      lang: java
      modules: [ "core", "ec2", "s3" ]
      version: "1.11.5"
"""
    roundTripsTo(input, output)
  }
}
