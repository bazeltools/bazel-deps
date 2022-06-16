package com.github.johnynek.bazel_deps

import cats.Traverse
import cats.data.NonEmptyList
import cats.implicits._
import com.github.johnynek.bazel_deps.IO.{Path, Result}
import org.slf4j.LoggerFactory

import scala.io.Source
import scala.util.{Failure, Success}

object Writer {

  // This changed from using Source.fromInputStream, as this prior method method could result in null values in a native-image.
  private[this] def loadResourceToString(path: String): String = {
    val is = getClass.getResourceAsStream(path)
    val outputBuffer = new java.io.ByteArrayOutputStream();
    val data = new Array[Byte](1024)
    @annotation.tailrec
    def go() {
      val nRead = is.read(data, 0, data.length)
      if (nRead != -1) {
        outputBuffer.write(data, 0, nRead)
        go
      }
    }
    go()
    outputBuffer.flush();
    new String(outputBuffer.toByteArray())
  }
  private lazy val jarArtifactBackend = loadResourceToString(
    "/templates/jar_artifact_backend.bzl"
  )

  private lazy val externalWorkspaceBackend = loadResourceToString(
    "/templates/external_workspace_backend.bzl"
  )

  sealed abstract class TargetsError {
    def message: String
  }
  object TargetsError {
    case class BadExport(
        uv: UnversionedCoordinate,
        unknownExports: List[(MavenGroup, ArtifactOrProject)]
    ) extends TargetsError {
      private def unknowns = unknownExports
        .map { case (g, a) => g.asString + ":" + a.asString }
        .mkString(", ")
      def message =
        s"Could not find explicit exports named by: ${uv.asString}: $unknowns"
    }

    case class CircularExports(
        duplicate: UnversionedCoordinate,
        path: List[UnversionedCoordinate]
    ) extends TargetsError {
      def message = "circular exports graph: " + (duplicate :: path)
        .map(_.asString)
        .mkString(", ")
    }
  }

  private[this] val logger = LoggerFactory.getLogger("Writer")

  case class DataSource(
      sha1: Option[String],
      sha256: Option[String],
      repository: Option[String],
      urls: List[String]
  )

  case class ArtifactReplacement(
      lang: String,
      bazelTarget: String
  )

  case class ArtifactEntry(
      artifact: String,
      version: String,
      lang: String,
      binaryJar: Option[DataSource],
      sourceJar: Option[DataSource],
      resolutionComment: Option[String],
      deps: List[String],
      exports: List[String], // "com/amazonaws:jmespath_java"
      replacementData: Option[ArtifactReplacement] = None
  )

  private def concreteToArtifactEntry(
      coord: MavenCoordinate,
      g: Graph[MavenCoordinate, Unit],
      duplicates: Map[UnversionedCoordinate, Set[Edge[MavenCoordinate, Unit]]],
      shas: Map[MavenCoordinate, ResolvedShasValue],
      model: Model
  ): ArtifactEntry = {
    val servers = model.getOptions.getResolvers.map(s => (s.id, s.url)).toMap
    val lang = language(g, model)
    val prefix = model.getOptions.getNamePrefix

    val isRoot = model.dependencies.roots(coord)

    val binaryJar = shas.get(coord).map { sha =>
      DataSource(
        sha1 = sha.binaryJar.sha1.map(_.toHex),
        sha256 = sha.binaryJar.sha256.map(_.toHex),
        repository = servers.get(sha.binaryJar.serverId),
        urls = List(sha.binaryJar.url.toList).flatten
      )
    }

    val sourceJar = shas.get(coord).flatMap(_.sourceJar).map { sourceJar =>
      DataSource(
        sha1 = sourceJar.sha1.map(_.toHex),
        sha256 = sourceJar.sha256.map(_.toHex),
        repository = servers.get(sourceJar.serverId),
        urls = List(sourceJar.url.toList).flatten
      )
    }
    def replaced(m: MavenCoordinate): Boolean =
      model.getReplacements.get(m.unversioned).isDefined

    val resolutionComment = duplicates.get(coord.unversioned).map { vs =>
      val status =
        if (isRoot) s"fixed to ${coord.version.asString}"
        else if (vs.map(_.destination.version).max == coord.version)
          s"promoted to ${coord.version.asString}"
        else s"downgraded to ${coord.version.asString}"

      s"""# duplicates in ${coord.unversioned.asString} $status\n""" +
        vs.filterNot(e => replaced(e.source))
          .map { e =>
            s"""# - ${e.source.asString} wanted version ${e.destination.version.asString}\n"""
          }
          .toSeq
          .sorted
          .mkString("")
    }

    val deps = g.hasSource(coord).toList

    val manualExports = model.dependencies
      .exportedUnversioned(coord.unversioned, model.getReplacements)
      .right
      .get

    val l = lang(coord.unversioned).asString
    ArtifactEntry(
      artifact = coord.unversioned.asString,
      version = coord.version.asString,
      lang = l,
      binaryJar = binaryJar,
      sourceJar = sourceJar,
      resolutionComment = resolutionComment,
      deps = deps.map(_.destination.unversioned.asString),
      exports = manualExports.map(_.asString)
    )
  }

  def artifactEntries(
      g: Graph[MavenCoordinate, Unit],
      duplicates: Map[UnversionedCoordinate, Set[Edge[MavenCoordinate, Unit]]],
      shas: Map[MavenCoordinate, ResolvedShasValue],
      model: Model
  ): Either[NonEmptyList[TargetsError], List[ArtifactEntry]] = {

    /** Check that all the exports are well-defined TODO make sure to write
      * targets for replaced nodes
      */
    val badExports =
      g.nodes.toList.flatMap { c =>
        val uv = c.unversioned
        model.dependencies.exportedUnversioned(
          uv,
          model.getReplacements
        ) match {
          case Left(baddies) =>
            List(TargetsError.BadExport(c.unversioned, baddies))
          case Right(_) => Nil
        }
      }

    val check = badExports match {
      case h :: tail => Left(NonEmptyList(h, tail))
      case Nil       => Right(())
    }
    check.map { _ =>
      def replaced(m: MavenCoordinate): Boolean =
        model.getReplacements.get(m.unversioned).isDefined

      /** Here are all the explicit artifacts
        */

      g.nodes.toIterator.map { m =>
        val concrete = concreteToArtifactEntry(m, g, duplicates, shas, model)
        model.getReplacements.get(m.unversioned) match {
          case Some(replacement) =>
            concrete.copy(
              replacementData = Some(
                ArtifactReplacement(
                  lang = replacement.lang.asReversableString,
                  bazelTarget = replacement.target.asString
                )
              )
            )
          case None => concrete
        }
      }.toList
    }
  }

  def language(
      g: Graph[MavenCoordinate, Unit],
      model: Model
  ): UnversionedCoordinate => Language = {

    /** Here are all the explicit artifacts
      */
    val uvToVerExplicit = g.nodes.map { c => (c.unversioned, c) }.toMap

    val rootScalaArtifacts = Set(
      "scala-library",
      "scala-compiler",
      "scala-reflect"
    )

    val scalaLang = model.getOptions.getLanguages
      .collectFirst { case Language.Scala(v, _) =>
        Language.Scala(v, false): Language
      }

    val langCache =
      scala.collection.mutable.Map[UnversionedCoordinate, Language]()
    def lang(u: UnversionedCoordinate): Language = langCache.getOrElseUpdate(
      u, {
        import Language.{Java, Scala}

        val rootScalaEntry = for {
          l <- scalaLang
          grp = u.group.asString
          artifactId = u.artifact.artifactId
          packaging = u.artifact.packaging
          if packaging == "jar" || packaging == ""
          if rootScalaArtifacts.contains(artifactId)
          if grp == "org.scala-lang"
        } yield {
          l
        }

        rootScalaEntry.getOrElse {
          model.dependencies.languageOf(u) match {
            case Some(l) => l
            case None =>
              Label.replaced(u, model.getReplacements) match {
                case Some((_, l)) => l
                case None         =>
                  // if you have any scala dependencies, you have to be handled by the
                  // scala rule for now, otherwise we say java
                  g.hasSource(uvToVerExplicit(u))
                    .iterator
                    .map(_.destination)
                    .map { c => lang(c.unversioned) }
                    .collectFirst { case s @ Scala(v, _) =>
                      val mangled = s.removeSuffix(u.asString).isDefined
                      Scala(v, mangled)
                    }
                    .getOrElse(Java)
              }
          }
        }
      }
    )

    { m => lang(m) }
  }
}
