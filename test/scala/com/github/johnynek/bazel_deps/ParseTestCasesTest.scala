package com.github.johnynek.bazel_deps

import com.github.johnynek.bazel_deps.ParseTestUtil._
import org.scalatest.FunSuite
class ParseTestCasesTest extends FunSuite {

  test("test regressions") {
    import Language.Java

    // This has a single sub-project, which we don't minimize into this form
    val model = Model(
      Dependencies(
        Map(
          MavenGroup("n2rr") ->
            Map(
              ArtifactOrProject("zmup") -> ProjectRecord(
                Java,
                Some(Version("019")),
                Some(Set(Subproject("wcv"))),
                Some(Set((MavenGroup("j9szw4"), ArtifactOrProject("i")))),
                None,
                None,
                None,
                None
              )
            )
        )
      ),
      Some(Replacements(Map())),
      None
    )

    law(model)

    val model1 = Model(
      Dependencies.empty,
      Some(Replacements.empty),
      Some(
        Options(
          None,
          None,
          None,
          Some(ResolverCache.Local),
          Some(NamePrefix("y")),
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          None
        )
      )
    )
    // println(model1.toDoc.render(70))
    law(model1)
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
      modules: [ "cache", "collection", "events" ]
      version: "6.29.0"
    util-codec:
      lang: scala
      version: "6.26.0"
    util-core:
      lang: scala
      version: "6.26.0"
    util-stats:
      lang: scala
      version: "6.26.0"
"""
    roundTripsTo(input, output)
  }

  // See #183, which removes this guarantee.
  ignore("maximize the project name") {
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

  test("Do not collapse when incompatible") {
    val input = """
dependencies:
   com.twitter:
    chill:
      lang: scala
      version: "0.8.4"
    chill-algebird:
      lang: scala
      version: "0.8.4"
      exports:
        - "com.twitter:chill"
    chill-scrooge:
      lang: scala
      version: "0.8.4"
      exports:
        - "com.twitter:chill"
"""
    val output = """dependencies:
  com.twitter:
    chill:
      lang: scala
      version: "0.8.4"
    chill-algebird:
      exports:\u0020
        - "com.twitter:chill"
      lang: scala
      version: "0.8.4"
    chill-scrooge:
      exports:\u0020
        - "com.twitter:chill"
      lang: scala
      version: "0.8.4"
"""

    roundTripsTo(input, output)
  }
}
