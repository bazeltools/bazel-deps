package com.github.johnynek.bazel_deps

import cats.data.Validated
import io.circe.Decoder.Result
import io.circe.{Decoder, Error, HCursor, Json, KeyDecoder, Parser}
import io.circe.generic.auto
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success, Try}

case class GradleLockDependency(
    locked: Option[String],
    project: Option[Boolean],
    transitive: Option[List[String]]
)

object GradleLockDependency {
  private val logger = LoggerFactory.getLogger("GradleLockDependency")

  sealed trait VersionState
  case object EqualVersionSpecified extends VersionState
  case object LeftVersion extends VersionState
  case object RightVersion extends VersionState

  private def resolveVersions(versionConflictPolicy: VersionConflictPolicy, dependencyName: String)(
      left: Option[String],
      right: Option[String]
  ): Try[VersionState] = {
    (left, right) match {
      case (_, _) if (left == right) => Success(EqualVersionSpecified)
      case (Some(_), None)                => Success(LeftVersion)
      case (None, Some(_))                => Success(RightVersion)
      case (Some(l), Some(r)) =>
        versionConflictPolicy
          .resolve(None, Set(Version(l), Version(r))) match {
            case Validated.Valid(v) =>
              val vstr = v.asString
              logger.debug(
                s"Using ${versionConflictPolicy} for $dependencyName with versions $l and $r picking $vstr"
              )

              val ver =
                if (vstr == l) LeftVersion
                else RightVersion

              Success(ver)
            case Validated.Invalid(iv) =>
              Failure(new Exception(s"For $dependencyName unable to combine versions $l and $r. Error: $iv"))
          }
    }
  }

  private val unit = Success(())

  def mergeGradleLockDeps(versionConflictPolicy: VersionConflictPolicy): TryMerge[GradleLockDependency] =
    new TryMerge[GradleLockDependency] {
      def tryMerge(
        debugName: Option[String],
        left: GradleLockDependency,
        right: GradleLockDependency
      ): Try[GradleLockDependency] = {
        lazy val mergedDependencies = Some(
          (left.transitive.getOrElse(Nil) ::: right.transitive.getOrElse(Nil))
            .distinct
            .sorted
        ).filter(_.nonEmpty)

        for {
          _ <- if (left.project == right.project) unit
            else Failure(
              new Exception(
                s"Unable to merge due to incompatible project setting in $debugName, had $left, $right"
              )
            )
          v <- resolveVersions(versionConflictPolicy, debugName.getOrElse("Unknown"))(left.locked, right.locked)
        } yield (v match {
            case EqualVersionSpecified | LeftVersion =>
              GradleLockDependency(
                locked = left.locked,
                project = left.project,
                transitive = mergedDependencies
              )
            case RightVersion =>
              GradleLockDependency(
                locked = right.locked,
                project = right.project, // note right.project == left.project or we don't reach here
                transitive = mergedDependencies
              )
          })
    }
  }

  implicit val gradleLockDependencyDecoder: Decoder[GradleLockDependency] =
    auto.exportDecoder[GradleLockDependency].instance
}
