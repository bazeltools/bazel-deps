package com.github.johnynek.bazel_deps

import cats.data.Xor
import io.circe.{ Decoder, KeyDecoder, Json, Error, Parser }
import io.circe.generic.auto

object Decoders {
  implicit val versionDecoder: Decoder[Version] = stringWrapper(Version(_))
  implicit val subprojDecoder: Decoder[Subproject] = stringWrapper(Subproject(_))
  implicit val dirnameDecoder: Decoder[DirectoryName] = stringWrapper(DirectoryName(_))
  implicit val targetDecoder: Decoder[BazelTarget] = stringWrapper(BazelTarget(_))
  implicit val transitivityDecoder: Decoder[Transitivity] =
    Decoder.decodeString.emap {
      case "exports" => Xor.right(Transitivity.Exports)
      case "runtime_deps" => Xor.right(Transitivity.RuntimeDeps)
      case other => Xor.left(s"unrecogized transitivity: $other")
    }
  implicit val groupArtDecoder: Decoder[(MavenGroup, ArtifactOrProject)] =
    Decoder.decodeString.emap { s =>
      s.split(':') match {
        case Array(g, a) => Xor.right((MavenGroup(g), ArtifactOrProject(a)))
        case _ => Xor.left(s"did not find exactly one ':' in $s")
      }
    }
  implicit val resolverDecorder: Decoder[MavenServer] =
    Decoder.decodeMapLike[Map, String, String].emap { smap =>
      def expect(k: String): Xor[String, String] =
        smap.get(k) match {
          case None =>
            Xor.left(s"expected key: $k, but only found ${smap.keys}")
          case Some(s) =>
            Xor.right(s)
        }

      val goodKeys = Set("id", "url", "type")
      lazy val badKeys = smap.keySet -- goodKeys
      for {
        id <- expect("id")
        url <- expect("url")
        contentType = smap.getOrElse("type", "default")
        _ <- if (badKeys.isEmpty) Xor.right(()) else Xor.left(s"unexpected keys: $badKeys")
      } yield MavenServer(id, contentType, url)
    }


  implicit val mavenGroupKey: KeyDecoder[MavenGroup] = KeyDecoder.instance { s =>
    Some(MavenGroup(s))
  }
  implicit val artifactOrProjKey: KeyDecoder[ArtifactOrProject] = KeyDecoder.instance { s =>
    Some(ArtifactOrProject(s))
  }

  implicit def vcpDecoder: Decoder[VersionConflictPolicy] =
    Decoder.decodeString.emap {
      case "fixed" => Xor.right(VersionConflictPolicy.Fixed)
      case "fail" => Xor.right(VersionConflictPolicy.Fail)
      case "highest" => Xor.right(VersionConflictPolicy.Highest)
      case other => Xor.left(s"unknown version conflict policy: $other")
    }

  implicit def optionsDecoder: Decoder[Options] = {
    implicit val versionLang: Decoder[Language] =
      Decoder.decodeString.emap {
        case "java" => Xor.right(Language.Java)
        case s if s.startsWith("scala:") =>
          s.split(':') match {
            case Array("scala", version) => Xor.right(Language.Scala(Version(version), true))
            case other => Xor.left(s"could not parse language: $s")
          }
        case "scala" => Xor.left("must declare a scala version. e.g. \"scala:2.11.8\"")
        case other => Xor.left(s"unknown language: $other")
      }

    auto.exportDecoder[Options].instance
  }

  private case class OptionsFirst(
    dependencies: Json,
    replacements: Option[Json],
    options: Option[Options])

  private def modelDecoder: Decoder[(Options, Decoder[Model])] =
    auto.exportDecoder[OptionsFirst].instance.map { justOpts =>
      val opts = justOpts.options.getOrElse(Options.default)

      implicit val lang: Decoder[Language] =
        Decoder.decodeString.emap {
          case "java" => Xor.right(Language.Java : Language)
          case "scala" =>
            opts.getLanguages
              .collectFirst { case Language.Scala(v, _) => Language.Scala(v, true): Language } match {
                case None => Xor.left(s"scala not listed in options: ${opts.languages}")
                case Some(l) => Xor.right(l)
              }
          case "scala/unmangled" =>
            opts.getLanguages
              .collectFirst { case Language.Scala(v, _) => Language.Scala(v, false): Language } match {
                case None => Xor.left(s"scala not listed in options: ${opts.languages}")
                case Some(l) => Xor.right(l)
              }
          case other => Xor.left(s"unknown language: $other"): Xor[String, Language]
        }

      implicit val rrD = auto.exportDecoder[ReplacementRecord].instance
      implicit val repD = Decoder
        .decodeMapLike[Map, MavenGroup, Map[ArtifactOrProject , ReplacementRecord]]
        .map(Replacements(_))

      implicit val prD = auto.exportDecoder[ProjectRecord].instance
      implicit val deps = Decoder
        .decodeMapLike[Map, MavenGroup, Map[ArtifactOrProject , ProjectRecord]]
        .map(Dependencies(_))

      (opts, auto.exportDecoder[Model].instance)
    }

  def decodeModel(p: Parser, str: String): Xor[Error, Model] =
    p.decode(str)(modelDecoder).flatMap { case (_, modDec) =>
      // we read twice, first to get the options, then parsing in the context of the options
      p.decode(str)(modDec)
    }

  private def stringWrapper[T](fn: String => T): Decoder[T] =
    Decoder.decodeString.map(fn)
}
