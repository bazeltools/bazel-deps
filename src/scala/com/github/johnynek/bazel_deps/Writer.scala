package com.github.johnynek.bazel_deps

import IO.{Path, Result}
import cats.Traverse
import cats.std.list._
import scala.util.{ Failure, Success, Try }

object Writer {

  def createBuildFiles(ts: List[Target]): Result[Int] = {
    val pathGroups = ts.groupBy(_.name.path).toList

    Traverse[List].traverseU(pathGroups) {
      case (filePath, ts) =>
        def data = ts.sortBy(_.name.name)
          .map(_.toBazelString)
          .mkString("", "\n\n", "\n")

          for {
            b <- IO.exists(filePath)
            _ <- if (b) IO.const(false) else IO.mkdirs(filePath)
            _ <- IO.writeUtf8(filePath.child("BUILD"), data)
          } yield ()
    }
      .map(_.size)
  }

  def workspace(g: Graph[MavenCoordinate, Unit],
    duplicates: Map[UnversionedCoordinate, Set[Version]],
    shas: Map[MavenCoordinate, Try[Sha1Value]],
    model: Model): String = {
    val nodes = g.nodes

    def replaced(m: MavenCoordinate): Boolean = model.getReplacements.get(m.unversioned).isDefined

    val lang = language(g, model)
    val lines = nodes.filterNot(replaced)
      .toList
      .sortBy(_.asString)
      .map { case coord@MavenCoordinate(g, a, v) =>
        val isRoot = model.dependencies.roots(coord)
        val shaStr = shas.get(coord) match {
          case Some(Success(sha)) => s""", "sha1": "${sha.toHex}""""
          case Some(Failure(err)) =>
            System.err.println(s"failed to find sha of ${coord.asString}: $err")
            throw err
          case None => ""
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
        val actual = Label.externalJar(l, coord.unversioned)
        def kv(key: String, value: String): String =
          s""""$key": "$value""""
        List(s"""$comment    callback({${kv("artifact", coord.asString)}""",
             s"""${kv("lang", l.asString)}$shaStr""",
             s"""${kv("name", coord.unversioned.toBazelRepoName)}""",
             s"""${kv("actual", actual.asStringFrom(Path(Nil)))}""",
             s"""${kv("bind", coord.unversioned.toBindingName)}})""").mkString(", ")
      }
      .mkString("\n")
    s"""def maven_dependencies(callback):\n$lines\n"""
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
          Target(lang, Label.localTarget(pathInRoot, u, lang), Set.empty, Set(lab), Set.empty)
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
              (Label.parse(u.bindTarget), langFn(u))
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
          Set.empty,
          exports ++ uvexports,
          runtime_deps -- uvexports)
      })
      def targetFor(u: UnversionedCoordinate): Target =
        replacedTarget(u).getOrElse(coordToTarget(u))

      Right(allUnversioned.iterator.map(targetFor).toList)
    }
  }
}

case class Label(workspace: Option[String], path: Path, name: String) {
  def asStringFrom(p: Path): String = {
    val nmPart = if (name.isEmpty) "" else s":$name"
    if (workspace.isEmpty && path == p) nmPart
    else {
      val ws = workspace.fold("") { nm => s"@$nm" }
      path.parts match {
        case Nil => s"$ws//$nmPart"
        case ps => ps.mkString(s"$ws//", "/", s"$nmPart")
      }
    }
  }
}

object Label {
  def parse(str: String): Label = {
    val ws = if (str(0) == '@') Some(str.tail.takeWhile(_ != '/')) else None
    val wsLen = ws.fold(0)(_.length)
    val pathAndTarg = str.drop(1 + wsLen).dropWhile(_ == '/')
    val pathStr = pathAndTarg.takeWhile(_ != ':')
    val target = pathAndTarg.drop(1 + pathStr.length)
    Label(ws, Path(pathStr.split('/').toList), target)
  }
  def externalJar(lang: Language, u: UnversionedCoordinate): Label = lang match {
    case Language.Java => Label(Some(u.toBazelRepoName), Path(List("jar")), "")
    // If we know we have a scala jar, just use ":file" to be sure we can deal with macros
    case Language.Scala(_, _) => Label(Some(u.toBazelRepoName), Path(List("jar")), "file")
  }

  def replaced(u: UnversionedCoordinate, r: Replacements): Option[(Label, Language)] =
    r.get(u).map { rr =>
      (Label.parse(rr.target.asString), rr.lang)
    }

  def localTarget(pathToRoot: List[String], m: UnversionedCoordinate, lang: Language): Label = {
    val p = Path(pathToRoot ::: (m.group.asString.map {
      case '.' => '/'
      case '-' => '_'
      case other => other
    }.mkString
    .split('/')
    .toList))

    val artName = lang match {
      case Language.Java => m.artifact.asString
      case s@Language.Scala(_, true) =>
        s.removeSuffix(m.artifact.asString) match {
          case Some(n) => n
          case None => sys.error(s"scala coordinate: ${m.asString} does not have correct suffix for $s")
        }
      case Language.Scala(_, false) => m.artifact.asString
    }

    val name = artName.map {
      case '.' => '_'
      case '-' => '_'
      case oth => oth
    }.mkString
    Label(None, p, name)
  }
}

case class Target(
  lang: Language,
  name: Label,
  deps: Set[Label],
  exports: Set[Label],
  runtimeDeps: Set[Label]) {

  def toBazelString: String = {
  /**
   * e.g.
   * scala_library(name = "foo",
   *   deps = [ ],
   *   exports = [ ],
   *   runtime_deps = [ ],
   *   visibility = ["//visibility:public"])
   */
    def labelList(outerIndent: Int, key: String, l: Set[Label]): String = {
      if (l.isEmpty) ""
      else {
        val prefix = s"$key = ["
        val indent = prefix.length
        val spaced = List.fill(outerIndent + indent)(' ').mkString
        l.map { label =>
          val str = label.asStringFrom(name.path)
          s""""$str""""
        }.toList.sorted.mkString("[", s",\n$spaced", "]")
      }
    }
    val langName = lang match {
      case Language.Java => "java"
      case Language.Scala(_, _) => "scala"
    }
    val header = s"${langName}_library("
    def sortKeys(items: List[(String, String)]): String = {
      // everything has a name
      val nm = s"""name = "${name.name}""""
      if (items.isEmpty) { header + nm + ")" }
      else {
        val sorted = items.sortBy(_._1).filter(_._2.nonEmpty)
        val prefixIndent = List.fill(header.length)(' ').mkString
        (nm :: (sorted.map { case (k, v) => s"$prefixIndent$k = $v" }))
          .mkString(header, ",\n", ")")
      }
    }
    val indent = header.length
    sortKeys(List(
      "visibility" ->  """["//visibility:public"]""",
      "deps" -> labelList(indent, "deps", deps),
      "exports" -> labelList(indent, "exports", exports),
      "runtime_deps" -> labelList(indent, "runtime_deps", runtimeDeps)))
  }
}
