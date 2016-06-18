package com.github.johnynek.bazel_deps

import org.scalatest.FunSuite

class ModelTest extends FunSuite {
  test("specific versions sort correctly") {
    assert(VersionConflictPolicy
      .Highest
      .resolve(None, Set(Version("11.0.2"), Version("18.0"))) === Right(Version("18.0")))

    assert(VersionConflictPolicy
      .Highest
      .resolve(Some(Version("18.0")), Set(Version("11.0.2"))) === Right(Version("18.0")))
  }
}
