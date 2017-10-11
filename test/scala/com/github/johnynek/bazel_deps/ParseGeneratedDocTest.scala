package com.github.johnynek.bazel_deps

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks._
import ParseTestUtil._

class ParseGeneratedDocTest extends FunSuite {
  test("parse randomly generated Model.toDoc") {
    // this test is slow and takes a lot of memory sadly
    implicit val generatorDrivenConfig =
      PropertyCheckConfig(minSuccessful = 200)

    forAll(ModelGenerators.modelGen)(law _)
  }

}
