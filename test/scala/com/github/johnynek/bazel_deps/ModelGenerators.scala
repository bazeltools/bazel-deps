package com.github.johnynek.bazel_deps

import org.scalacheck.{ Arbitrary, Gen }

object ModelGenerators {

  def join[A, B](a: Gen[A], b: => Gen[B]): Gen[(A, B)] = a.flatMap { aa => b.map((aa, _)) }

  val mavenPart: Gen[String] = Gen.identifier

  val subprojGen: Gen[Subproject] = mavenPart.map(Subproject(_))
  val langGen: Gen[Language] = Gen.oneOf(Language.Java, Language.Scala(Version("2.11.8"), true))
  val mavenGroupGen: Gen[MavenGroup] = mavenPart.map(MavenGroup(_))
  val artifactOrProjGen: Gen[ArtifactOrProject] = mavenPart.map(ArtifactOrProject(_))

  def projectRecordGen(langs: List[Language]): Gen[ProjectRecord] = for {
    lang <- Gen.oneOf(langs)
    v <- Gen.option(Gen.listOfN(3, Gen.choose('0', '9')).map { l => Version(l.mkString) })
    sub <- Gen.choose(0, 6)
    exp <- Gen.choose(0, 3)
    exc <- Gen.choose(0, 3)
    m <- Gen.option(Gen.listOfN(sub, subprojGen).map(_.toSet))
    exports <- Gen.option(Gen.listOfN(exp, join(mavenGroupGen, artifactOrProjGen)).map(_.toSet))
    exclude <- Gen.option(Gen.listOfN(exc, join(mavenGroupGen, artifactOrProjGen)).map(_.toSet))
  } yield ProjectRecord(lang, v, m, exports, exclude)

  def depGen(o: Options): Gen[Dependencies] = {
    def artMap = Gen.mapOf(join(artifactOrProjGen, projectRecordGen(o.getLanguages))).map(_.take(30))
    Gen.mapOf(join(mavenGroupGen, artMap)).map { m => Dependencies(m.take(100)) }
  }

  val genBazelTarget: Gen[BazelTarget] =
    Gen.listOf(Gen.identifier).map { l => BazelTarget(l.mkString("//", "/", "")) }

  def rrGen(langs: List[Language]): Gen[ReplacementRecord] =
    for {
      l <- Gen.oneOf(langs)
      t <- genBazelTarget
    } yield ReplacementRecord(l, t)

  def replacementGen(langs: List[Language]): Gen[Replacements] = {
    def artMap = Gen.mapOf(join(artifactOrProjGen, rrGen(langs))).map(_.take(30))
    Gen.mapOf(join(mavenGroupGen, artMap)).map { m => Replacements(m.take(100)) }
  }

  val mavenServerGen: Gen[MavenServer] = for {
    id <- Gen.identifier
    ct <- Gen.identifier
    urlParts <- Gen.choose(0, 4)
    path <- Gen.listOfN(urlParts, Gen.identifier)
    url <- Gen.identifier.map { m => path.mkString(s"http://$m", "/", "") }
  } yield MavenServer(id, ct, url)

  val optionGen: Gen[Options] = for {
    vcp <- Gen.option(Gen.oneOf(VersionConflictPolicy.Fail, VersionConflictPolicy.Fixed, VersionConflictPolicy.Highest))
    dir <- Gen.option(Gen.identifier.map(DirectoryName(_)))
    langs <- Gen.option(Gen.listOf(langGen).map(_.toSet))
    res <- Gen.option(Gen.listOf(mavenServerGen))
    trans <- Gen.option(Gen.oneOf(Transitivity.RuntimeDeps, Transitivity.Exports))
    heads <- Gen.option(Gen.listOf(Gen.identifier))
    cache <- Gen.option(Gen.oneOf(ResolverCache.Local, ResolverCache.BazelOutputBase))
  } yield Options(vcp, dir, langs, res, trans, heads, cache)

  val modelGen: Gen[Model] = for {
    o <- Gen.option(optionGen)
    opts = o.getOrElse(Options.default)
    d <- depGen(opts)
    r <- Gen.option(replacementGen(opts.getLanguages.toList))
  } yield Model(d, r, o)
}
