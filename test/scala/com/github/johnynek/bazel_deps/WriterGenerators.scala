package com.github.johnynek.bazel_deps

import org.scalacheck.Gen

object WriterGenerators {
  import ModelGenerators._

  val labelGen: Gen[Label] = for {
    workspace <- Gen.option(Gen.identifier)
    path <- Gen.listOf(Gen.identifier).map { l => FS.Path(l) }
    name <- Gen.identifier
  } yield Label(workspace, path, name)

  val datasourceGen: Gen[DataSource] = for {
    sha1 <- Gen.option(Gen.identifier)
    sha256 <- Gen.option(Gen.identifier)
    bytes <- Gen.option(Gen.choose(0L, Int.MaxValue.toLong + 10L))
    repository <- Gen.option(Gen.identifier)
    urls <- Gen.listOf(Gen.identifier)
  } yield DataSource(
    sha1 = sha1,
    sha256 = sha256,
    file_size_bytes = bytes,
    repository = repository,
    urls = urls
  )

  val targetGen: Gen[ArtifactEntry] = for {
    language <- langGen
    name <- labelGen
    deps <- Gen.listOf(Gen.identifier).map(_.toSet)
    binaryJar <- Gen.option(datasourceGen)
    sourceJar <- Gen.option(datasourceGen)
    exports <- Gen.listOf(Gen.identifier).map(_.toSet)
    artifact <- Gen.identifier
    version <- Gen.identifier

  } yield ArtifactEntry(
    artifact = artifact,
    version = version,
    lang = language.asString,
    binaryJar = binaryJar,
    sourceJar = sourceJar,
    resolutionComment = None,
    deps = deps.toList,
    exports = exports.toList,
    replacementData = None
  )
}
