package com.github.johnynek.bazel_deps
import java.io.File

import cats.implicits._

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks._
import WriterGenerators._

import scala.util.{Failure, Success}

class TargetTestTest extends FunSuite {
  test("Test we can serialize and round trip via the string format") {
    val separator = "|||"
    forAll(targetGen) { target =>
      val rt = target.listStringEncoding(separator).flatMap { e =>
        Target.fromListStringEncoding(separator, e)
      }

      val rtV = rt.foldMap(IO.fileSystemExec(new File("/tmp"))) match {
        case Failure(err) =>
          fail("Failure during IO:", err)
        case Success(result) =>
          result
      }
      assert(rtV === target)
    }
  }
}
