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
    m <- Gen.option(Gen.listOf(subprojGen))
    exports <- Gen.option(Gen.listOf(join(mavenGroupGen, artifactOrProjGen)))
    exclude <- Gen.option(Gen.listOf(join(mavenGroupGen, artifactOrProjGen)))
  } yield ProjectRecord(lang, v, m, exports, exclude)

  def depGen(o: Options): Gen[Dependencies] = {
    def artMap = Gen.mapOf(join(artifactOrProjGen, projectRecordGen(o.getLanguages)))
    Gen.mapOf(join(mavenGroupGen, artMap)).map(Dependencies(_))
  }

  def replacementGen: Gen[Replacements] = ???

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
    langs <- Gen.option(Gen.listOf(langGen))
    res <- Gen.option(Gen.listOf(mavenServerGen))
    trans <- Gen.option(Gen.oneOf(Transitivity.RuntimeDeps, Transitivity.Exports))
    heads <- Gen.option(Gen.listOf(Gen.identifier))
  } yield Options(vcp, dir, langs, res, trans, heads)

  val modelGen: Gen[Model] = for {
    o <- Gen.option(optionGen)
    opts = o.getOrElse(Options.default)
    d <- depGen(opts)
    //r <- Gen.option(replacementGen) todo
    r = None
  } yield Model(d, r, o)
}
