package com.github.johnynek.bazel_deps

import cats.data.Xor
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import io.circe.jackson.CirceJsonModule
import io.circe.{Decoder, Json, ParsingFailure, Parser}
import scala.util.control.NonFatal

/**
 * To use this, implement a Decoder for your type, or in
 * many cases:
 * import io.circe.generic.auto._
 * will work
 */
object Yaml extends Parser {
  private[this] val mapper = new ObjectMapper(new YAMLFactory()).registerModule(CirceJsonModule)
  private[this] val factory = mapper.getFactory
  override def parse(input: String): Xor[ParsingFailure, Json] =
    try {
      Xor.right(mapper.readValue(factory.createParser(input), classOf[Json]))
    } catch {
      case NonFatal(error) => Xor.left(ParsingFailure(error.getMessage, error))
    }
}
