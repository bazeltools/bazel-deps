package com.github.johnynek.bazel_deps

import cats.Traverse
import cats.data.NonEmptyList
import cats.implicits._
import com.github.johnynek.bazel_deps.IO.{Path, Result}
import org.slf4j.LoggerFactory

import scala.io.Source

object Writer {
  private lazy val jarArtifactBackend = Source.fromInputStream(
    getClass.getResource("/templates/jar_artifact_backend.bzl").openStream()).mkString

  sealed abstract class TargetsError {
    def message: String
  }
  object TargetsError {
    case class BadExport(uv: UnversionedCoordinate, unknownExports: List[(MavenGroup, ArtifactOrProject)]) extends TargetsError {
      private def unknowns = unknownExports.map { case (g, a) => g.asString + ":" + a.asString }.mkString(", ")
      def message = s"Could not find explicit exports named by: ${uv.asString}: $unknowns"
    }

    case class CircularExports(duplicate: UnversionedCoordinate, path: List[UnversionedCoordinate]) extends TargetsError {
      def message = "circular exports graph: " + (duplicate :: path).map(_.asString).mkString(", ")
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

  def compareBuildFiles(buildHeader: String, ts: List[Target], formatter: BuildFileFormatter, buildFileName: String): Result[List[IO.FileComparison]] = {
    val pathGroups = ts.groupBy(_.name.path).toList

    Traverse[List].traverse(pathGroups) {
      case (filePath, ts) =>
        def data(bf: IO.Path) = buildFileContents(bf, buildHeader, ts, formatter)
        val bf = filePath.child(buildFileName)
        IO.compare(bf, data(bf))
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

    s"""# Do not edit. bazel-deps autogenerates this file from $depsFile.
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

  def language(g: Graph[MavenCoordinate, Unit],
    model: Model): UnversionedCoordinate => Language = {
    /**
     * Here are all the explicit artifacts
     */
    val uvToVerExplicit = g.nodes.map { c => (c.unversioned, c) }.toMap

    val langCache = scala.collection.mutable.Map[UnversionedCoordinate, Language]()
    def lang(u: UnversionedCoordinate): Language = langCache.getOrElseUpdate(u, {
      import Language.{Java, Scala}

      model.dependencies.languageOf(u) match {
        case Some(l) => l
        case None =>
          Label.replaced(u, model.getReplacements) match {
            case Some((_, l)) => l
            case None =>
              // if you have any scala dependencies, you have to be handled by the
              // scala rule for now, otherwise we say java
              g.hasSource(uvToVerExplicit(u))
                .iterator
                .map(_.destination)
                .map { c => lang(c.unversioned) }
                .collectFirst {
                  case s@Scala(v, _) =>
                    val mangled = s.removeSuffix(u.asString).isDefined
                    Scala(v, mangled)
                }
                .getOrElse(Java)
          }
      }
    })

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
        else if( ! model.options.flatMap { _.strictVisibility.map(_.enabled) }.getOrElse(true)) Target.Visibility.Public
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
                visibility = visibility(u),
                exports = Set(lab),
                jars = Set.empty,
                licenses = licenses)
            case Language.Kotlin =>
              Target(lang,
                kind = Target.Import,
                name = Label.localTarget(pathInRoot, u, lang),
                visibility = visibility(u),
                exports = Set(lab),
                jars = Set.empty,
                licenses = licenses)
            case _: Language.Scala =>
              Target(lang,
                kind = Target.Library,
                name = Label.localTarget(pathInRoot, u, lang),
                visibility = visibility(u),
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
