package com.github.johnynek.bazel_deps

import org.scalatest.FunSuite

class TargetTest extends FunSuite {
  test("Target legacyShow") {
    val t = Target(Language.Java, Label.parse("//foo:bar"),
      deps = Set(Label.parse("//foo:baz"), Label.parse("//bang")),
      exports = Set.empty,
      runtimeDeps = Set(Label.parse("//bang:bop")))

    val legacy = Target.legacyShow.show(t)
    val expected = """java_library(name = "bar",
                     |             deps = ["//bang",
                     |                     ":baz"],
                     |             runtime_deps = ["//bang:bop"],
                     |             visibility = ["//visibility:public"])""".stripMargin('|')
    assert(legacy == expected)
  }
}
