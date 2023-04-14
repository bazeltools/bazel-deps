package com.github.johnynek.bazel_deps

import cats.syntax.either._
import io.circe.Decoder.Result
import io.circe.{Decoder, Error, HCursor, Json, KeyDecoder, Parser}
import io.circe.generic.auto

object Decoders {
  implicit val gradleLockDependencyDecoder: Decoder[GradleLockDependency] =
    auto.exportDecoder[GradleLockDependency].instance
  implicit val gradleLockFileDecoder: Decoder[GradleLockFile] =
    auto.exportDecoder[GradleLockFile].instance
  implicit val versionDecoder: Decoder[Version] = stringWrapper(Version(_))
  implicit val processorClassDecoder: Decoder[ProcessorClass] = stringWrapper(
    ProcessorClass(_)
  )
  implicit val subprojDecoder: Decoder[Subproject] = stringWrapper(
    Subproject(_)
  )
  implicit val targetDecoder: Decoder[BazelTarget] = stringWrapper(
    BazelTarget(_)
  )

  implicit val resolverCacheDecoder: Decoder[ResolverCache] =
    Decoder.decodeString.emap {
      case "local"             => Right(ResolverCache.Local)
      case "bazel_output_base" => Right(ResolverCache.BazelOutputBase)
      case other               => Left(s"unrecogized resolverCache: $other")
    }

  implicit val namePrefixDecoder: Decoder[NamePrefix] = stringWrapper(
    NamePrefix(_)
  )
  implicit val groupArtDecoder: Decoder[(MavenGroup, ArtifactOrProject)] =
    Decoder.decodeString.emap { s =>
      s.split(':') match {
        case Array(g, a, p, c) =>
          Right((MavenGroup(g), ArtifactOrProject(a, p, Some(c))))
        case Array(g, a, p) =>
          Right((MavenGroup(g), ArtifactOrProject(a, p, None)))
        case Array(g, a) => Right((MavenGroup(g), ArtifactOrProject(a)))
        case _ =>
          Left(
            s"$s did not match expected maven coord format <groupId>:<artifactId>[:<extension>[:<classifier>]]"
          )
      }
    }
  implicit val resolverDecoder: Decoder[DependencyServer] =
    Decoder[Map[String, String]].emap { smap =>
      def expect(k: String): Either[String, String] =
        smap.get(k) match {
          case None =>
            Left(s"expected key: $k, but only found ${smap.keys}")
          case Some(s) =>
            Right(s)
        }

      val serverType = smap.getOrElse("serverType", "maven")
      val serverTypeParsed =
        if (serverType == "maven" || serverType == "ivy") Right(serverType)
        else
          Left(
            s"Unknown dependency server type: '$serverType', expected 'maven' or 'ivy'"
          )
      lazy val mavenServer = {
        val goodKeys = Set("id", "url", "type", "serverType")
        lazy val badKeys = smap.keySet -- goodKeys
        for {
          id <- expect("id")
          url <- expect("url")
          contentType = smap.getOrElse("type", "default")
          _ <-
            if (badKeys.isEmpty) Right(())
            else Left(s"unexpected keys: $badKeys")
        } yield {
          MavenServer(id, contentType, url)
        }
      }

      lazy val ivyServer = {
        val goodKeys =
          Set("id", "url", "ivyPattern", "ivyArtifactPattern", "serverType")
        lazy val badKeys = smap.keySet -- goodKeys
        for {
          id <- expect("id")
          url <- expect("url")
          ivyPattern <- expect("ivyPattern")
          ivyArtifactPattern <- expect("ivyArtifactPattern")
          _ <-
            if (badKeys.isEmpty) Right(())
            else Left(s"unexpected keys: $badKeys")
        } yield {
          IvyServer(id, url, ivyPattern, ivyArtifactPattern)
        }
      }

      serverTypeParsed.flatMap { tpe =>
        if (tpe == "maven") {
          mavenServer
        } else {
          ivyServer
        }
      }

    }

  implicit val mavenGroupKey: KeyDecoder[MavenGroup] = KeyDecoder.instance {
    s =>
      Some(MavenGroup(s))
  }
  implicit val artifactOrProjKey: KeyDecoder[ArtifactOrProject] =
    KeyDecoder.instance { s =>
      Some(ArtifactOrProject(s))
    }

  implicit def vcpDecoder: Decoder[VersionConflictPolicy] =
    Decoder.decodeString.emap {
      case "fixed"   => Right(VersionConflictPolicy.Fixed)
      case "fail"    => Right(VersionConflictPolicy.Fail)
      case "highest" => Right(VersionConflictPolicy.Highest)
      case other     => Left(s"unknown version conflict policy: $other")
    }

  implicit def optionsDecoder: Decoder[Options] = {
    implicit val versionLang: Decoder[Language] =
      Decoder.decodeString.emap {
        case "java"   => Right(Language.Java)
        case "kotlin" => Right(Language.Kotlin)
        case s if s.startsWith("scala:") =>
          s.split(':') match {
            case Array("scala", version) =>
              Right(Language.Scala(Version(version), true))
            case other => Left(s"could not parse language: $s")
          }
        case "scala" =>
          Left("must declare a scala version. e.g. \"scala:2.11.8\"")
        case other => Left(s"unknown language: $other")
      }

    implicit val resolverTypeDecoder: Decoder[ResolverType] = {
      Decoder.decodeString.emap {
        case "aether"   => Right(ResolverType.Aether)
        case "coursier" => Right(ResolverType.Coursier)
        case "gradle"   => Right(ResolverType.Gradle.empty)
        case other      => Left(s"unrecogized resolverType: $other")
      }
    }
    implicit val transitivityDecoder: Decoder[Transitivity] = {
      Decoder.decodeString.emap{
        case "runtime_deps" => Right(Transitivity.RuntimeDeps)
        case "exports" => Right(Transitivity.Exports)
        case other => Left(s"unrecogized transitivity: $other")
      }
    }

    implicit val directoryNameDecoder: Decoder[DirectoryName] = {
      Decoder.decodeString.map(str => DirectoryName(str))
    }

    implicit val strictVisibilityDecoder: Decoder[StrictVisibility] = {
      Decoder.decodeBoolean.map(bool => StrictVisibility(bool))
    }

    implicit val gradleDecoder =
      auto.exportDecoder[ResolverType.Gradle].instance
    val baseOptions = auto.exportDecoder[Options].instance
    new Decoder[Options] {
      override def apply(c: HCursor): Result[Options] = {
        baseOptions(c) match {
          case Right(b) => {
            b.resolverType match {
              case Some(x) =>
                x match {
                  case g: ResolverType.Gradle =>
                    c.get[Option[ResolverType.Gradle]]("resolverOptions").map {
                      optV =>
                        optV
                          .map { inner => b.copy(resolverType = Some(inner)) }
                          .getOrElse(b)
                    }
                  case _ => Right(b)
                }
              case None => Right(b)
            }
          }
          case Left(a) => Left(a)
        }
      }
    }
  }

  private case class OptionsFirst(
      dependencies: Json,
      replacements: Option[Json],
      options: Option[Options]
  )

  private def modelDecoder: Decoder[(Options, Decoder[Model])] =
    auto.exportDecoder[OptionsFirst].instance.map { justOpts =>
      val opts = justOpts.options.getOrElse(Options.default)

      implicit val lang: Decoder[Language] =
        Decoder.decodeString.emap {
          case "java"   => Right(Language.Java: Language)
          case "kotlin" => Right(Language.Kotlin: Language)
          case "scala" =>
            opts.getLanguages
              .collectFirst { case Language.Scala(v, _) =>
                Language.Scala(v, true): Language
              } match {
              case None =>
                Left(s"scala not listed in options: ${opts.languages}")
              case Some(l) => Right(l)
            }
          case "scala/unmangled" =>
            opts.getLanguages
              .collectFirst { case Language.Scala(v, _) =>
                Language.Scala(v, false): Language
              } match {
              case None =>
                Left(s"scala not listed in options: ${opts.languages}")
              case Some(l) => Right(l)
            }
          case other =>
            Left(s"unknown language: $other"): Either[String, Language]
        }

      implicit val rrD = auto.exportDecoder[ReplacementRecord].instance
      implicit val repD =
        Decoder[Map[MavenGroup, Map[ArtifactOrProject, ReplacementRecord]]]
          .map(Replacements(_))

      implicit val prD = auto.exportDecoder[ProjectRecord].instance
      implicit val deps =
        Decoder[Map[MavenGroup, Map[ArtifactOrProject, ProjectRecord]]]
          .map(Dependencies(_))

      (opts, auto.exportDecoder[Model].instance)
    }

  def decodeModel(p: Parser, str: String): Either[Error, Model] =
    p.decode(str)(modelDecoder).flatMap { case (_, modDec) =>
      // we read twice, first to get the options, then parsing in the context of the options
      p.decode(str)(modDec)
    }

  def decodeGradleLockFile(
      p: Parser,
      str: String
  ): Either[Error, GradleLockFile] =
    p.decode(str)(gradleLockFileDecoder)

  private def stringWrapper[T](fn: String => T): Decoder[T] =
    Decoder.decodeString.map(fn)
}
