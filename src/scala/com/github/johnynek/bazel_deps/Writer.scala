package com.github.johnynek.bazel_deps

import IO.{Path, Result}
import cats.Traverse
import cats.implicits._
import scala.util.{ Failure, Success, Try }

object Writer {

  /**
   * Takes a BUILD file path and generated contents, and returns the formatted version of those contents (e.g. with
   * buildifier).
   */
  type BuildFileFormatter = ((IO.Path, String) => String)

  private val buildFileName = "BUILD"

  private def buildFileContents(buildFilePath: IO.Path, buildHeader: String, ts: List[Target], formatter: BuildFileFormatter): String = {
    def withNewline(s: String): String =
      if (s.isEmpty) ""
      else s + "\n"

    formatter(buildFilePath, ts.sortBy(_.name.name)
      .map(_.toDoc.render(60))
      .mkString(withNewline(buildHeader), "\n\n", "\n"))
  }

  def createBuildFiles(buildHeader: String, ts: List[Target], formatter: BuildFileFormatter): Result[Int] = {
    val pathGroups = ts.groupBy(_.name.path).toList

    Traverse[List].traverseU(pathGroups) {
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

  def compareBuildFiles(buildHeader: String, ts: List[Target], formatter: BuildFileFormatter): Result[List[IO.FileComparison]] = {
    val pathGroups = ts.groupBy(_.name.path).toList

    Traverse[List].traverseU(pathGroups) {
      case (filePath, ts) =>
        def data(bf: IO.Path) = buildFileContents(bf, buildHeader, ts, formatter)
        val bf = filePath.child(buildFileName)
        IO.compare(bf, data(bf))
    }
  }

  def workspace(depsFile: String, g: Graph[MavenCoordinate, Unit],
    duplicates: Map[UnversionedCoordinate, Set[Edge[MavenCoordinate, Unit]]],
    shas: Map[MavenCoordinate, Try[ResolvedSha1Value]],
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
        val (shaStr, serverStr) = shas.get(coord) match {
          case Some(Success(sha)) =>
            val hex = sha.sha1Value.toHex
            val serverUrl = servers.getOrElse(sha.serverId, "")
            (s""", "sha1": "${hex}"""", s""", "repository": "${serverUrl}"""")
          case Some(Failure(err)) =>
            System.err.println(s"failed to find sha of ${coord.asString}: $err")
            throw err
          case None => ("", "")
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
              }.mkString("")
          case None =>
            ""
        }
        val l = lang(coord.unversioned)
        val actual = Label.externalJar(l, coord.unversioned, prefix)
        def kv(key: String, value: String): String =
          s""""$key": "$value""""
        List(s"""$comment    {${kv("artifact", coord.asString)}""",
             s"""${kv("lang", l.asString)}$shaStr$serverStr""",
             s"""${kv("name", coord.unversioned.toBazelRepoName(prefix))}""",
             s"""${kv("actual", actual.asStringFrom(Path(Nil)))}""",
             s"""${kv("bind", coord.unversioned.toBindingName(prefix))}},""").mkString(", ")
      }
      .mkString("\n")
    s"""# Do not edit. bazel-deps autogenerates this file from ${depsFile}.
        |
        |def declare_maven(hash):
        |    native.maven_jar(
        |        name = hash["name"],
        |        artifact = hash["artifact"],
        |        sha1 = hash["sha1"],
        |        repository = hash["repository"]
        |    )
        |    native.bind(
        |        name = hash["bind"],
        |        actual = hash["actual"]
        |    )
        |
        |def list_dependencies():
        |    return [
        |$lines
        |    ]
        |
        |def maven_dependencies(callback = declare_maven):
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
      import Language.{ Java, Scala }

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
    model: Model): Either[List[UnversionedCoordinate], List[Target]] = {
    /**
     * Check that all the exports are well-defined
     * TODO make sure to write targets for replaced nodes
     */
    val badExports =
      g.nodes.filter { c =>
        model.dependencies.exportedUnversioned(c.unversioned, model.getReplacements).isLeft
      }

    /**
     * Here are all the explicit artifacts
     */
    val uvToVerExplicit = g.nodes.map { c => (c.unversioned, c) }.toMap
    /**
     * Here are any that are replaced, they may not appear above:
     */
    val uvToRep = model.getReplacements.unversionedToReplacementRecord

    /**
     * Here are all the unversioned artifacts we need to create targets for:
     */
    val allUnversioned: Set[UnversionedCoordinate] = uvToVerExplicit.keySet.union(uvToRep.keySet)

    if (badExports.nonEmpty) Left(badExports.toList.map(_.unversioned))
    else {
      val rootName = model.getOptions.getThirdPartyDirectory
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
                exports = Set(lab),
                jars = Set.empty)
            case _: Language.Scala =>
              Target(lang,
                kind = Target.Library,
                name = Label.localTarget(pathInRoot, u, lang),
                exports = Set(lab),
                jars = Set.empty)
          }

        }
      /*
       * We make 1 label for each target, the path
       * and name are derived from the MavenCoordinate
       */
      val cache = scala.collection.mutable.Map[UnversionedCoordinate, Target]()
      def coordToTarget(u: UnversionedCoordinate): Target = cache.getOrElseUpdate(u, {
        val deps = g.hasSource(uvToVerExplicit(u))
        val depLabels = deps.map { e => targetFor(e.destination.unversioned).name }
        val (lab, lang) =
          Label.replaced(u, model.getReplacements)
            .getOrElse {
              (Label.parse(u.bindTarget(model.getOptions.getNamePrefix)), langFn(u))
            }
        // Build explicit exports, no need to add these to runtime deps
        val uvexports = model.dependencies
          .exportedUnversioned(u, model.getReplacements).right.get
          .map(targetFor(_).name)

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
              exports = (exports + lab) ++ uvexports,
              jars = Set.empty,
              runtimeDeps = runtime_deps -- uvexports,
              processorClasses = getProcessorClasses(u),
              licenses = licenses)
          case _: Language.Scala =>
            Target(lang,
              kind = Target.Import,
              name = Label.localTarget(pathInRoot, u, lang),
              exports = exports ++ uvexports,
              jars = Set(lab),
              runtimeDeps = runtime_deps -- uvexports,
              processorClasses = getProcessorClasses(u),
              licenses = licenses)
        }


      })
      def targetFor(u: UnversionedCoordinate): Target =
        replacedTarget(u).getOrElse(coordToTarget(u))
      def getProcessorClasses(u: UnversionedCoordinate): Set[ProcessorClass] =
        (for {
          m <- model.dependencies.toMap.get(u.group)
          projectRecord <- m.get(ArtifactOrProject(u.artifact.asString))
        } yield projectRecord.processorClasses).flatten.getOrElse(Set.empty)

      Right(allUnversioned.iterator.map(targetFor).toList)
    }
  }
}
