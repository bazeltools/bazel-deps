package com.github.johnynek.bazel_deps

import cats.Traverse
import cats.data.NonEmptyList
import cats.implicits._
import com.github.johnynek.bazel_deps.IO.{Path, Result}
import org.slf4j.LoggerFactory

import scala.io.Source
import scala.util.{Failure, Success}

case class DataSource(
    sha1: Option[String],
    sha256: Option[String],
    file_size_bytes: Option[Long],
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

  /**
   * Takes a BUILD file path and generated contents, and returns the formatted version of those contents (e.g. with
   * buildifier).
   */
  type BuildFileFormatter = ((IO.Path, String) => String)

  private def buildFileContents(buildFilePath: IO.Path, buildHeader: String, ts: List[Target], formatter: BuildFileFormatter): String = {
    def withNewline(s: String): String =
      if (s.isEmpty) ""
      else s + "\n"

    formatter(buildFilePath, ts.sortBy(_.name.name)
      .map(_.toDoc.render(60))
      .mkString(withNewline(buildHeader), "\n\n", "\n"))
  }

  def compareBuildFiles(buildHeader: String, ts: List[Target], formatter: BuildFileFormatter, buildFileName: String): Result[List[IO.FileComparison]] = {
    val pathGroups = ts.groupBy(_.name.path).toList

    Traverse[List].traverse(pathGroups) {
      case (filePath, ts) =>
        def data(bf: IO.Path) = buildFileContents(bf, buildHeader, ts, formatter)

        val bf = filePath.child(buildFileName)
        IO.compare(bf, data(bf))
    }
  }
  def createBuildFilesAndTargetFile(buildHeader: String, ts: List[Target], targetFileOpt: Option[IO.Path], enable3rdPartyInRepo: Boolean, thirdPartyDirectory: DirectoryName, formatter: BuildFileFormatter, buildFileName: String): Result[Int] = {
    val with3rdpartyPrinted = if (enable3rdPartyInRepo) {
      createBuildFiles(buildHeader, ts, formatter, buildFileName)
    } else IO.const(0)

    val withTargetFilePrinted = targetFileOpt match {
      case Some(tfp) => createBuildTargetFile(buildHeader, ts, tfp, thirdPartyDirectory)
      case None => IO.const(0)
    }

    with3rdpartyPrinted.flatMap(e => withTargetFilePrinted.map { u => u + e })
  }

  def createBuildFiles(buildHeader: String, ts: List[Target], formatter: BuildFileFormatter, buildFileName: String): Result[Int] = {
    val pathGroups = ts.groupBy(_.name.path).toList

    Traverse[List].traverse(pathGroups) {
      case (filePath, ts) =>
        def data(bf: IO.Path) = buildFileContents(bf, buildHeader, ts, formatter)

        for {
          b <- IO.exists(filePath)
          _ <- if (b) IO.const(false) else IO.mkdirs(filePath)
          bf = filePath.child(buildFileName)
          _ <- IO.writeUtf8(bf, data(bf))
        } yield ()
    }
      .map(_.size)
  }

  def createBuildTargetFile(buildHeader: String, ts: List[Target], tfp: Path, thirdPartyDirectory: DirectoryName): Result[Int] =
    for {
      b <- IO.exists(tfp.parent)
      _ <- if (b) IO.const(false) else IO.mkdirs(tfp.parent)
      buildFileContent <- createBuildTargetFileContents(buildHeader, ts, thirdPartyDirectory)
      _ <- IO.writeUtf8(tfp, buildFileContent)
    } yield ts.size

  def createBuildTargetFileContents(buildHeader: String, ts: List[Target], thirdPartyDirectory: DirectoryName): Result[String] = {
    val separator = "|||"
    val encodingVersion = 1
    Traverse[List].traverse(ts
      .sortBy(_.toString)) { target =>
      def kv(key: String, value: String, prefix: String = ""): String =
        s"""$prefix"$key": "$value""""

      def kvOpt(key: String, valueOpt: Option[String], prefix: String = ""): String = valueOpt match {
        case Some(value) => kv(key, value, prefix)
        case None => ""
      }

      def kListV(key: String, values: List[String], prefix: String = ""): String = {
        val v = values.map { e => "\"" + e + "\"" }.mkString(",")
        s"""$prefix"$key": [$v]"""
      }

      val targetName = target.name
      val key = s"${targetName.path.asString}:${targetName.name}"
      for {
        targetEncoding <- target.listStringEncoding(separator)
      } yield kListV(s"$key", targetEncoding)
    }.map { lines: List[String] =>

      s"""# Do not edit. bazel-deps autogenerates this file from.
         |$externalWorkspaceBackend
         |
         |def build_header():
         | return ""${"\"" + buildHeader + "\""}""
         |
         |def list_target_data_separator():
         | return "${separator}"
         |
         |def list_target_data():
         |    return {
         |${lines.mkString(",\n")}
         | }
         |
         |
         |def build_external_workspace(name):
         |  return build_external_workspace_from_opts(name = name, target_configs = list_target_data(), separator = list_target_data_separator(), build_header = build_header())
         |
         |""".stripMargin
    }
  }

  def workspace(depsFile: String, g: Graph[MavenCoordinate, Unit],
                duplicates: Map[UnversionedCoordinate, Set[Edge[MavenCoordinate, Unit]]],
                shas: Map[MavenCoordinate, ResolvedShasValue],
                model: Model): String = {
    val nodes = g.nodes

    def replaced(m: MavenCoordinate): Boolean = model.getReplacements.get(m.unversioned).isDefined

    val servers = model.getOptions.getResolvers.map(s => (s.id, s.url)).toMap
    val lang = language(g, model)
    val prefix = model.getOptions.getNamePrefix
    val lines = nodes.filterNot(replaced)
      .toList
      .sortBy(_.asString)
      .map { case coord@MavenCoordinate(g, a, v) =>
        val isRoot = model.dependencies.roots(coord)

        def kv(key: String, value: String, prefix: String = ""): String =
          s"""$prefix"$key": "$value""""

        def kvOpt(key: String, valueOpt: Option[String], prefix: String = ""): String = valueOpt match {
          case Some(value) => kv(key, value, prefix)
          case None => ""
        }

        val (sha1Str, sha256Str, serverStr, remoteUrl) = shas.get(coord) match {
          case Some(sha) =>
            val sha1Str = kvOpt("sha1", sha.binaryJar.sha1.map(_.toHex), ", ")
            val sha256Str = kvOpt("sha256", sha.binaryJar.sha256.map(_.toHex), ", ")
            // val url = sha.url
            val serverUrlStr = kvOpt("repository", servers.get(sha.binaryJar.serverId), ", ")
            val urlStr = kvOpt("url", sha.binaryJar.url, ", ")

            (sha1Str, sha256Str, serverUrlStr, urlStr)
          case None => ("", "", "", "")
        }

        val sourceStr = shas.get(coord).flatMap(_.sourceJar) match {
          case Some(sourceJar) =>
            val sha1Str = kvOpt("sha1", sourceJar.sha1.map(_.toHex))
            val sha256Str = kvOpt("sha256", sourceJar.sha256.map(_.toHex), ", ")
            // val url = sha.url
            val serverUrlStr = kvOpt("repository", servers.get(sourceJar.serverId), ", ")
            val urlStr = kvOpt("url", sourceJar.url, ", ")

            (sha1Str, sha256Str, serverUrlStr, urlStr)
            s""", "source": {$sha1Str$sha256Str$serverUrlStr$urlStr} """
          case None => ""
        }

        val comment = duplicates.get(coord.unversioned) match {
          case Some(vs) =>
            val status =
              if (isRoot) s"fixed to ${v.asString}"
              else if (vs.map(_.destination.version).max == v) s"promoted to ${v.asString}"
              else s"downgraded to ${v.asString}"

            s"""# duplicates in ${coord.unversioned.asString} $status\n""" +
              vs.filterNot(e => replaced(e.source)).map { e =>
                s"""# - ${e.source.asString} wanted version ${e.destination.version.asString}\n"""
              }.toSeq.sorted.mkString("")
          case None =>
            ""
        }
        val l = lang(coord.unversioned)
        val actual = Label.externalJar(l, coord.unversioned, prefix)
        List(s"""$comment    {${kv("artifact", coord.asString)}""",
          s"""${kv("lang", l.asString)}$sha1Str$sha256Str$serverStr$remoteUrl$sourceStr""",
          s"""${kv("name", coord.unversioned.toBazelRepoName(prefix))}""",
          s"""${kv("actual", actual.fromRoot)}""",
          s"""${kv("bind", coord.unversioned.toBindingName(prefix))}},""").mkString(", ")
      }
      .mkString("\n")

    val ifAuth: String => String =
      if (model.hasAuthFile) identity
      else _ => ""

    val jarArtifactImpl =
      s"""${
        if (model.hasAuthFile)
          s"""|load("@bazel_tools//tools/build_defs/repo:utils.bzl", "read_netrc", "use_netrc")
              |
              |def _jar_artifact_impl(ctx):
              |    netrc = read_netrc(ctx, "${model.getAuthFile.get}")
              |    auth = use_netrc(netrc, ctx.attr.urls, {})
              |""".stripMargin
        else "def _jar_artifact_impl(ctx):"
      }
         |    jar_name = "%s.jar" % ctx.name
         |    ctx.download(
         |        output = ctx.path("jar/%s" % jar_name),
         |        url = ctx.attr.urls,
         |        sha256 = ctx.attr.sha256,
         |        executable = False${ifAuth(",\n        auth = auth")}
         |    )
         |    src_name = "%s-sources.jar" % ctx.name
         |    srcjar_attr = ""
         |    has_sources = len(ctx.attr.src_urls) != 0
         |    if has_sources:${ifAuth("\n        src_auth = use_netrc(netrc, ctx.attr.src_urls, {})")}
         |        ctx.download(
         |            output = ctx.path("jar/%s" % src_name),
         |            url = ctx.attr.src_urls,
         |            sha256 = ctx.attr.src_sha256,
         |            executable = False${ifAuth(",\n            auth = src_auth")}
         |        )""".stripMargin

    s"""# Do not edit. bazel-deps autogenerates this file from $depsFile.
       |$jarArtifactImpl
       |$jarArtifactBackend
       |
       |def list_dependencies():
       |    return [
       |$lines
       |    ]
       |
       |def maven_dependencies(callback = jar_artifact_callback):
       |    for hash in list_dependencies():
       |        callback(hash)
       |""".stripMargin
  }

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
        file_size_bytes = sha.binaryJar.fileSizeBytes,
        repository = servers.get(sha.binaryJar.serverId),
        urls = List(sha.binaryJar.url.toList).flatten
      )
    }

    val sourceJar = shas.get(coord).flatMap(_.sourceJar).map { sourceJar =>
      DataSource(
        sha1 = sourceJar.sha1.map(_.toHex),
        sha256 = sourceJar.sha256.map(_.toHex),
        file_size_bytes = sourceJar.fileSizeBytes,
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

  def targets(g: Graph[MavenCoordinate, Unit],
              model: Model): Either[NonEmptyList[TargetsError], List[Target]] = {
    /**
     * Check that all the exports are well-defined
     * TODO make sure to write targets for replaced nodes
     */
    val badExports =
      g.nodes.toList.flatMap { c =>
        val uv = c.unversioned
        model.dependencies.exportedUnversioned(uv, model.getReplacements) match {
          case Left(baddies) => List(TargetsError.BadExport(c.unversioned, baddies))
          case Right(_) => Nil
        }
      }

    val check = badExports match {
      case h :: tail => Left(NonEmptyList(h, tail))
      case Nil => Right(())
    }

    type E[A] = Either[NonEmptyList[TargetsError], A]
    check.right.flatMap { _ =>

      /**
       * Here are all the explicit artifacts
       */
      val uvToVerExplicit = g.nodes.map { c => (c.unversioned, c) }.toMap
      /**
       * Here are any that are replaced, they may not appear above:
       */
      val uvToRep = model.getReplacements.unversionedToReplacementRecord

      val rootName = model.getOptions.getThirdPartyDirectory
      val thirdPartyVis = Target.Visibility.SubPackages(Label(None, Path(rootName.parts), ""))

      val allRootsUv = model.dependencies.roots.map(_.unversioned) | model.dependencies.unversionedRoots

      def visibility(uv: UnversionedCoordinate): Target.Visibility =
        if (allRootsUv(uv)) Target.Visibility.Public
        else if (!model.options.flatMap {
          _.strictVisibility.map(_.enabled)
        }.getOrElse(true)) Target.Visibility.Public
        else thirdPartyVis

      /**
       * Here are all the unversioned artifacts we need to create targets for:
       */
      val allUnversioned: Set[UnversionedCoordinate] = uvToVerExplicit.keySet.union(uvToRep.keySet)

      val licenses = model.getOptions.getLicenses
      val pathInRoot = rootName.parts

      val langFn = language(g, model)

      def replacedTarget(u: UnversionedCoordinate): Option[Target] =
        Label.replaced(u, model.getReplacements).map { case (lab, lang) =>
          // TODO: converge on using java_import instead of java_library:
          // https://github.com/johnynek/bazel-deps/issues/102
          lang match {
            case Language.Java =>
              Target(lang,
                kind = Target.Library,
                name = Label.localTarget(pathInRoot, u, lang),
                visibility = Target.Visibility.Public,
                exports = Set(lab),
                jars = Set.empty,
                licenses = licenses)
            case Language.Kotlin =>
              Target(lang,
                kind = Target.Import,
                name = Label.localTarget(pathInRoot, u, lang),
                visibility = Target.Visibility.Public,
                exports = Set(lab),
                jars = Set.empty,
                licenses = licenses)
            case _: Language.Scala =>
              Target(lang,
                kind = Target.Library,
                name = Label.localTarget(pathInRoot, u, lang),
                visibility = Target.Visibility.Public,
                exports = Set(lab),
                jars = Set.empty,
                licenses = licenses)
          }

        }

      /*
       * We make 1 label for each target, the path
       * and name are derived from the MavenCoordinate
       */
      val cache = scala.collection.mutable.Map[UnversionedCoordinate, Either[List[UnversionedCoordinate], Either[NonEmptyList[TargetsError], Target]]]()

      def coordToTarget(u: UnversionedCoordinate): Either[NonEmptyList[TargetsError], Target] = {

        def compute: Either[NonEmptyList[TargetsError], Target] = {
          val deps = g.hasSource(uvToVerExplicit(u)).toList

          def labelFor(u: UnversionedCoordinate): Either[NonEmptyList[TargetsError], Label] =
            targetFor(u).right.map(_.name)

          Traverse[List].traverse[E, Edge[MavenCoordinate, Unit], Label](deps) { e => labelFor(e.destination.unversioned) }.right.flatMap { depLabelList =>
            val depLabels = depLabelList.toSet
            val (lab, lang) =
              Label.replaced(u, model.getReplacements)
                .getOrElse {
                  (Label.parse(u.bindTarget(model.getOptions.getNamePrefix)), langFn(u))
                }
            // Build explicit exports, no need to add these to runtime deps
            Traverse[List].traverse[E, UnversionedCoordinate, Label](
              model
                .dependencies
                .exportedUnversioned(u, model.getReplacements).right.get
            )(labelFor(_))
              .right
              .map { uvexports =>

                val (exports, runtime_deps) = model.getOptions.getTransitivity match {
                  case Transitivity.Exports => (depLabels, Set.empty[Label])
                  case Transitivity.RuntimeDeps => (Set.empty[Label], depLabels)
                }

                // TODO: converge on using java_import instead of java_library:
                // https://github.com/johnynek/bazel-deps/issues/102
                lang match {
                  case Language.Java =>
                    Target(lang,
                      kind = Target.Library,
                      name = Label.localTarget(pathInRoot, u, lang),
                      visibility = visibility(u),
                      exports = if (u.artifact.packaging == "pom") {
                        exports
                      } else {
                        (exports + lab)
                      } ++ uvexports,
                      jars = Set.empty,
                      runtimeDeps = runtime_deps -- uvexports,
                      processorClasses = getProcessorClasses(u),
                      generatesApi = getGeneratesApi(u),
                      licenses = licenses,
                      generateNeverlink = getGenerateNeverlink(u))
                  case Language.Kotlin =>
                    Target(lang,
                      kind = Target.Import,
                      name = Label.localTarget(pathInRoot, u, lang),
                      visibility = visibility(u),
                      exports = exports ++ uvexports,
                      jars = Set(lab),
                      runtimeDeps = runtime_deps -- uvexports,
                      processorClasses = getProcessorClasses(u),
                      generatesApi = getGeneratesApi(u))
                  case _: Language.Scala =>
                    Target(lang,
                      kind = Target.Import,
                      name = Label.localTarget(pathInRoot, u, lang),
                      visibility = visibility(u),
                      exports = exports ++ uvexports,
                      jars = Set(lab),
                      runtimeDeps = runtime_deps -- uvexports,
                      processorClasses = getProcessorClasses(u),
                      generatesApi = getGeneratesApi(u),
                      licenses = licenses)
                }
              }
          }
        }

        cache.getOrElseUpdate(u, Left(Nil)) match {
          case Left(existing) if existing.contains(u) =>
            Left(NonEmptyList.of(TargetsError.CircularExports(u, existing)))
          case Left(existing) =>
            cache.update(u, Left(u :: existing))
            val res = compute
            cache.update(u, Right(res))
            res
          case Right(res) => res
        }
      }

      def targetFor(u: UnversionedCoordinate): Either[NonEmptyList[TargetsError], Target] =
        replacedTarget(u) match {
          case Some(t) => Right(t)
          case None => coordToTarget(u)
        }

      def getProcessorClasses(u: UnversionedCoordinate): Set[ProcessorClass] =
        (for {
          m <- model.dependencies.toMap.get(u.group)
          projectRecord <- m.get(ArtifactOrProject(u.artifact.asString))
        } yield projectRecord.processorClasses).flatten.getOrElse(Set.empty)

      def getGeneratesApi(u: UnversionedCoordinate): Boolean =
        (for {
          m <- model.dependencies.toMap.get(u.group)
          projectRecord <- m.get(ArtifactOrProject(u.artifact.asString))
        } yield projectRecord.generatesApi.getOrElse(false)).getOrElse(false)

      def getGenerateNeverlink(u: UnversionedCoordinate): Boolean =
        (for {
          m <- model.dependencies.toMap.get(u.group)
          projectRecord <- m.get(ArtifactOrProject(u.artifact.asString))
        } yield projectRecord.generateNeverlink.getOrElse(false)).getOrElse(false)

      Traverse[List].traverse[E, UnversionedCoordinate, Target](allUnversioned.toList)(targetFor(_))
    }
  }
}
