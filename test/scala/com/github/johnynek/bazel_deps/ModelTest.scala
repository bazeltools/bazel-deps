package com.github.johnynek.bazel_deps

import org.scalatest.FunSuite
import cats.data.Validated

class ModelTest extends FunSuite {
  test("specific versions sort correctly") {
    assert(VersionConflictPolicy
      .Highest
      .resolve(None, Set(Version("11.0.2"), Version("18.0"))) == Validated.valid(Version("18.0")))

    assert(VersionConflictPolicy
      .Highest
      .resolve(Some(Version("18.1")), Set(Version("11.0.2"))) == Validated.valid(Version("18.1")))
  }
  test("RC/M is less than final") {
    val ord = Ordering[Version]
    assert(ord.lt(Version("0.5.0-M2"), Version("0.5.0")), "0.5.0-M2 < 0.5.0")
    assert(ord.lt(Version("0.16.1-RC2"), Version("0.16.1")), "0.16.1-RC1 < 0.16.1")
    assert(ord.lt(Version("0.16.1-RC2"), Version("0.16.10-RC4")), "0.16.1-RC1 < 0.16.10-RC4")
    assert(ord.lt(Version("0.16.2-RC2"), Version("0.16.10-RC4")), "0.16.2-RC1 < 0.16.10-RC4")
  }
  test("a test array is sorted") {
    val sorted =
      List("1.0.9a", "1.0.9", "1.0.10", "2.0RC0", "2.0-rc1", "2.0rc2", "2.0", "3.1.4.2-M1", "3.1.4.2", "10.2")
        .map(Version(_))

    val rand = scala.util.Random.shuffle(sorted)
    assert(rand.sorted === sorted)
  }

  test("empty subproject is merged correctly with new submodule") {
    val lang = Language.Scala.default
    val deps = Dependencies(Map(
      MavenGroup("com.twitter") -> Map(
        ArtifactOrProject("finagle") -> ProjectRecord(lang, Some(Version("0.1")), Some(Set(Subproject(""), Subproject("core"))), None, None, None, None)
      )
    ))

    Dependencies.combine(
      VersionConflictPolicy.Highest,
      MavenCoordinate("com.twitter:finagle-stats:0.1").toDependencies(lang),
      deps
    ) match {
      case Validated.Invalid(str) => fail(s"couldn't combine added dep: $str")
      case Validated.Valid(combined) =>
        val str = """com.twitter:
                    |  finagle:
                    |    lang: scala
                    |    modules: [ "", "core", "stats" ]
                    |    version: "0.1"""".stripMargin('|')
        assert(combined.toDoc.render(100) == str)
    }
  }

  test("coordinate naming") {
    val uc = UnversionedCoordinate(MavenGroup("com.twitter"), MavenArtifactId("finagle-core"))
    assert(uc.asString == "com.twitter:finagle-core")
    assert(uc.toBazelRepoName(NamePrefix("")) == "com_twitter_finagle_core")
    assert(uc.toBindingName(NamePrefix("")) == "jar/com/twitter/finagle_core")
    assert(uc.bindTarget(NamePrefix("")) == "//external:jar/com/twitter/finagle_core")
    val np = NamePrefix("unique_")
    assert(uc.toBazelRepoName(np) == "unique_com_twitter_finagle_core")
    assert(uc.toBindingName(np) == "jar/unique_com/twitter/finagle_core")
    assert(uc.bindTarget(np) == "//external:jar/unique_com/twitter/finagle_core")
  }

  test("packaging and classifier are extracted properly parsed in MavenArtifactId") {
    def assertAll(id: MavenArtifactId, a: String, p: String, c: Option[String]) : Unit = {
      assert(id.artifactId == a)
      assert(id.packaging == p)
      assert(id.classifier == c)
    }

    assertAll(MavenArtifactId("foo"), "foo", "jar", None)
    assertAll(MavenArtifactId("foo:dll"), "foo", "dll", None)
    assertAll(MavenArtifactId("foo:dll:tests"), "foo", "dll", Some("tests"))
  }

  test("MavenArtifactId asString") {
    assert(MavenArtifactId("foo", "jar", Some("some-classifier")).asString == "foo:jar:some-classifier")
    assert(MavenArtifactId("foo", "dll", Some("some-classifier")).asString == "foo:dll:some-classifier")

    // rule: don't include packaging if packaging = jar and no classifier
    assert(MavenArtifactId("foo", "jar", None).asString == "foo")
    assert(MavenArtifactId("foo", "dll", None).asString == "foo:dll")

    List("foo:jar:some-classifier", "foo", "foo:dll", "foo:dll:some-classifier").foreach { s => {
      assert(MavenArtifactId(s).asString == s)
    }}

    assert(MavenArtifactId("foo:jar").asString == "foo")
  }

  test("MavenArtifactId addSuffix") {
    assert(MavenArtifactId("foo:dll:some-classifier").addSuffix("_1").asString == "foo_1:dll:some-classifier")
    assert(MavenArtifactId("foo:dll").addSuffix("_1").asString == "foo_1:dll")
    assert(MavenArtifactId("foo:jar:some-classifier").addSuffix("_1").asString == "foo_1:jar:some-classifier")

    // special: remove default packaging
    assert(MavenArtifactId("foo:jar").addSuffix("_1").asString == "foo_1")
  }
}
