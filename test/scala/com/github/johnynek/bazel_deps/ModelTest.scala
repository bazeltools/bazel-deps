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

  test("project record combination works") {
    def makeRec(artifact: String, module: Option[String]): (ArtifactOrProject, ProjectRecord) =
      (ArtifactOrProject(artifact), ProjectRecord(
        Language.Java,
        None,
        module.map { s => Set(Subproject(s)) },
        None,
        None))

    Dependencies.merge(makeRec("poi", None), makeRec("poi-ooxml", None)) match {
      case Some((ap, pr)) =>
        assert(ap == ArtifactOrProject("poi"))
        assert(pr.modules.get.map(_.asString) == Set("", "ooxml"))
      case None => fail("couldn't merge")
    }
  }

  test("empty subproject is merged correctly with new submodule") {
    val lang = Language.Scala.default
    val deps = Dependencies(Map(
      MavenGroup("com.twitter") -> Map(
        ArtifactOrProject("finagle") -> ProjectRecord(lang, Some(Version("0.1")), Some(Set(Subproject(""), Subproject("core"))), None, None)
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
}
