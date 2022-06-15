package com.github.johnynek.bazel_deps

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import io.circe.jackson.CirceJsonModule
import io.circe.{Decoder, Json, ParsingFailure, Parser}
import scala.util.control.NonFatal

/** To use this, implement a Decoder for your type, or in many cases: import
  * io.circe.generic.auto._ will work
  */
object Yaml extends Parser {
  private[this] val mapper =
    new ObjectMapper(new YAMLFactory()).registerModule(CirceJsonModule)
  private[this] val factory = mapper.getFactory
  override def parse(input: String): Either[ParsingFailure, Json] =
    try {
      Right(mapper.readValue(factory.createParser(input), classOf[Json]))
    } catch {
      case NonFatal(error) => Left(ParsingFailure(error.getMessage, error))
    }
}
