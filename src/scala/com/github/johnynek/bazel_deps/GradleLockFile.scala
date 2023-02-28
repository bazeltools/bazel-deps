package com.github.johnynek.bazel_deps

import io.circe.Decoder.Result
import io.circe.{Decoder, Error, HCursor, Json, KeyDecoder, Parser}
import io.circe.generic.auto

import scala.util.{Failure, Success, Try}

object GradleLockFile {
  implicit val gradleLockFileDecoder: Decoder[GradleLockFile] =
    auto.exportDecoder[GradleLockFile].instance

  implicit def mergeInst(implicit tm: TryMerge[GradleLockDependency]) = new TryMerge[GradleLockFile] {
    def tryMerge(
        debugName: Option[String],
        left: GradleLockFile,
        right: GradleLockFile
    ): Try[GradleLockFile] = {
      for {
        annotationProcessor <- TryMerge.tryMerge(
          debugName,
          left.annotationProcessor,
          right.annotationProcessor
        )
        compileClasspath <- TryMerge.tryMerge(
          debugName,
          left.compileClasspath,
          right.compileClasspath
        )
        resolutionRules <- TryMerge.tryMerge(
          debugName,
          left.resolutionRules,
          right.resolutionRules
        )
        runtimeClasspath <- TryMerge.tryMerge(
          debugName,
          left.runtimeClasspath,
          right.runtimeClasspath
        )
        testCompileClasspath <- TryMerge.tryMerge(
          debugName,
          left.testCompileClasspath,
          right.testCompileClasspath
        )
        testRuntimeClasspath <- TryMerge.tryMerge(
          debugName,
          left.testRuntimeClasspath,
          right.testRuntimeClasspath
        )
      } yield {
        GradleLockFile(
          annotationProcessor,
          compileClasspath,
          resolutionRules,
          runtimeClasspath,
          testCompileClasspath,
          testRuntimeClasspath
        )
      }
    }
  }
  def empty = GradleLockFile(None, None, None, None, None, None)

  def decodeGradleLockFile(
      p: Parser,
      str: String
  ): Either[Error, GradleLockFile] =
    p.decode[GradleLockFile](str)
}

case class GradleLockFile(
    annotationProcessor: Option[Map[String, GradleLockDependency]],
    compileClasspath: Option[Map[String, GradleLockDependency]],
    resolutionRules: Option[Map[String, GradleLockDependency]],
    runtimeClasspath: Option[Map[String, GradleLockDependency]],
    testCompileClasspath: Option[Map[String, GradleLockDependency]],
    testRuntimeClasspath: Option[Map[String, GradleLockDependency]]
)
