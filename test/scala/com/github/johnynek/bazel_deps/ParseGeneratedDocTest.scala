package com.github.johnynek.bazel_deps

import org.scalacheck.Gen
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.prop.Configuration.PropertyCheckConfiguration
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import ParseTestUtil._

class ParseGeneratedDocTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("parse randomly generated Model.toDoc") {
    // this test is slow and takes a lot of memory sadly
    implicit val generatorDrivenConfig =
      PropertyCheckConfiguration(minSuccessful = 50)

    forAll(ModelGenerators.modelGen)(law _)
  }

  property("Dependencies.normalize laws") {
    val genList = Gen.listOf(
      Gen.zip(
        ModelGenerators.artifactOrProjGen,
        ModelGenerators.projectRecordGen(Language.Java, Nil)
      )
    )

    forAll(genList) { lp =>
      val output = Dependencies.normalize(lp)
      assert(lp.size >= output.size)
      val flat1 = lp.flatMap { case (a, p) => p.flatten(a) }
      val flat2 = output.flatMap { case (a, p) => p.flatten(a) }
      assert(flat1.toSet == flat2.toSet)
    }
  }
}
