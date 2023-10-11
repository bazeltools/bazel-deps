package com.github.johnynek.bazel_deps

object ParseTestUtil {
  def decode(str: String): Model = {
    val Right(mod) = Decoders.decodeModel(Yaml, str)
    mod
  }

  def law(model: Model) = {
    val str = model.toDoc.render(70)
    val decoded = decode(str)
    // if (decoded != model) {
    //   println(str)
    //   println("------")
    //   println(decoded.toDoc.render(70))
    // }
    assert(decoded == model || decoded.flatten == model.flatten)
    assert(decoded.toDoc.render(70) == str)
  }

  def roundTripsTo(input: String, output: String) = {
    val mod = decode(input)
    val modStr = mod.toDoc.render(70)
    // assert(decode(modStr) === mod)
    // println(input)
    // println("------")
    // println(modStr)
    assert(modStr == output)
  }

}
