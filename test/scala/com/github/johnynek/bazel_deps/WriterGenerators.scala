package com.github.johnynek.bazel_deps

import org.scalacheck.Gen

object WriterGenerators {
  import ModelGenerators._

  val labelGen: Gen[Label] = for {
    workspace <- Gen.option(Gen.identifier)
    path <- Gen.listOf(Gen.identifier).map { l => IO.Path(l) }
    name <- Gen.identifier
  } yield Label(workspace, path, name)

  val targetGen: Gen[Target] = for {
    language <- langGen
    name <- labelGen
    visibility <- labelGen.flatMap { id =>
      Gen.oneOf(
        Target.Visibility.Public,
        Target.Visibility.SubPackages(id.copy(name = ""))
      )
    }
    kind <- Gen.oneOf(Target.Library, Target.Import, Target.Test, Target.Binary)
    deps <- Gen.listOf(labelGen).map(_.toSet)
    jars <- Gen.listOf(labelGen).map(_.toSet)
    sources <- Gen.oneOf(
      Target.SourceList.Empty,
      Target.SourceList.Explicit(Set("abcd")),
      Target.SourceList.Globs(List("*.a.*java"))
    )
    exports <- Gen.listOf(labelGen).map(_.toSet)
    runtimeDeps <- Gen.listOf(labelGen).map(_.toSet)
    processorClasses <- Gen.listOf(processorClassGen).map(_.toSet)
    generatesApi <- Gen.oneOf(true, false)
    licences <- Gen.listOf(Gen.identifier).map(_.toSet)
    generateNeverLink <- Gen.oneOf(true, false)
  } yield Target(
    language,
    name,
    visibility,
    kind,
    deps,
    jars,
    sources,
    exports,
    runtimeDeps,
    processorClasses,
    generatesApi,
    licences,
    generateNeverLink
  )
}
