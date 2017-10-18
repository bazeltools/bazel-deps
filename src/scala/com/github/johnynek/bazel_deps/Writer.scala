package com.github.johnynek.bazel_deps

import IO.{Path, Result}
import cats.Traverse
import cats.implicits._
import scala.util.{ Failure, Success, Try }

object Writer {

  private val buildFileName = "BUILD"

  private def buildFileContents(buildHeader: String, ts: List[Target]): String = {
    def withNewline(s: String): String =
      if (s.isEmpty) ""
      else s + "\n"

    ts.sortBy(_.name.name)
      .map(_.toDoc.render(60))
      .mkString(withNewline(buildHeader), "\n\n", "\n")
  }

  def createBuildFiles(buildHeader: String, ts: List[Target]): Result[Int] = {
    val pathGroups = ts.groupBy(_.name.path).toList

    Traverse[List].traverseU(pathGroups) {
      case (filePath, ts) =>
        def data = buildFileContents(buildHeader, ts)
        for {
          b <- IO.exists(filePath)
          _ <- if (b) IO.const(false) else IO.mkdirs(filePath)
          _ <- IO.writeUtf8(filePath.child(buildFileName), data)
        } yield ()
    }
      .map(_.size)
  }

  def compareBuildFiles(buildHeader: String, ts: List[Target]): Result[List[IO.FileComparison]] = {
    val pathGroups = ts.groupBy(_.name.path).toList

    Traverse[List].traverseU(pathGroups) {
      case (filePath, ts) =>
        def data = buildFileContents(buildHeader, ts)
        IO.compare(filePath.child(buildFileName), data)
    }
  }

  def workspace(g: Graph[MavenCoordinate, Unit],
    duplicates: Map[UnversionedCoordinate, Set[Version]],
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
              else if (vs.max == v) s"promoted to ${v.asString}"
              else s"downgraded to ${v.asString}"

            s"""# duplicates in ${coord.unversioned.asString} $status. Versions: ${vs.toList.sorted.map(_.asString).mkString(" ")}\n"""
          case None =>
            ""
        }
        val l = lang(coord.unversioned)
        val actual = Label.externalJar(l, coord.unversioned, prefix)
        def kv(key: String, value: String): String =
          s""""$key": "$value""""
        List(s"""$comment    callback({${kv("artifact", coord.asString)}""",
             s"""${kv("lang", l.asString)}$shaStr$serverStr""",
             s"""${kv("name", coord.unversioned.toBazelRepoName(prefix))}""",
             s"""${kv("actual", actual.asStringFrom(Path(Nil)))}""",
             s"""${kv("bind", coord.unversioned.toBindingName(prefix))}})""").mkString(", ")
      }
      .mkString("\n")
    s"""def declare_maven(hash):
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
        |def maven_dependencies(callback = declare_maven):
        |$lines
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
      val pathInRoot = rootName.parts

      val langFn = language(g, model)
      def replacedTarget(u: UnversionedCoordinate): Option[Target] =
        Label.replaced(u, model.getReplacements).map { case (lab, lang) =>
          Target(lang, Label.localTarget(pathInRoot, u, lang), exports = Set(lab))
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
          case Transitivity.Exports => (depLabels + lab, Set.empty[Label])
          case Transitivity.RuntimeDeps => (Set(lab), depLabels)
        }
        Target(lang,
          Label.localTarget(pathInRoot, u, lang),
          exports = exports ++ uvexports,
          runtimeDeps = runtime_deps -- uvexports)
      })
      def targetFor(u: UnversionedCoordinate): Target =
        replacedTarget(u).getOrElse(coordToTarget(u))

      Right(allUnversioned.iterator.map(targetFor).toList)
    }
  }
}
