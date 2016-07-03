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
        val fileBytes = ts.sortBy(_.name.name)
          .map(_.toBazelString)
          .mkString("", "\n\n", "\n")
          .getBytes("UTF-8")

          for {
            b <- IO.exists(filePath)
            _ <- if (b) IO.const(false) else IO.mkdirs(filePath)
            _ <- IO.write(filePath.child("BUILD"), fileBytes)
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
        s"""$comment    callback({"artifact": "${coord.asString}", "name": "${coord.toBazelRepoName}"$shaStr})"""
      }
      .mkString("\n")
    s"""def maven_dependencies(callback):\n$lines\n"""
  }

  def targets(g: Graph[MavenCoordinate, Unit],
    model: Model): List[Target] = {
    val rootName = model.getOptions.getThirdPartyDirectory
    val pathInRoot = rootName.parts

    val langCache = scala.collection.mutable.Map[MavenCoordinate, Language]()
    def language(c: MavenCoordinate): Language = langCache.getOrElseUpdate(c, {
      import Language.{ Java, Scala }

      model.dependencies.languageOf(c) match {
        case Some(l) => l
        case None =>
          Label.replaced(c, model.getReplacements) match {
            case Some((_, l)) => l
            case None =>
              // if you have any scala dependencies, you have to be handled by the
              // scala rule for now, otherwise we say java
              g.hasSource(c)
                .iterator
                .map(_.destination)
                .map(language(_))
                .collectFirst {
                  case s@Scala(v, _) =>
                    val mangled = s.removeSuffix(c.unversioned.asString).isDefined
                    Scala(v, mangled)
                }
                .getOrElse(Java)
          }
      }
    })

    /*
     * We make 1 label for each target, the path
     * and name are derived from the MavenCoordinate
     */
    val cache = scala.collection.mutable.Map[MavenCoordinate, Target]()
    def coordToTarget(c: MavenCoordinate): Target = cache.getOrElseUpdate(c, {
      val deps = g.hasSource(c)
      val depLabels = deps.map { e => coordToTarget(e.destination).name }.toList
      val (lab, lang) =
        Label.replaced(c, model.getReplacements)
          .getOrElse {
            val lang = language(c)
            (Label.externalJar(lang, c), lang)
          }
      val exports = lab :: depLabels
      Target(lang, Label.localTarget(pathInRoot, c, lang), Nil, exports, Nil)
    })

    g.nodes.iterator.map(coordToTarget).toList
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
  def externalJar(lang: Language, m: MavenCoordinate): Label = lang match {
    case Language.Java => Label(Some(m.toBazelRepoName), Path(List("jar")), "")
    // If we know we have a scala jar, just use ":file" to be sure we can deal with macros
    case Language.Scala(_, _) => Label(Some(m.toBazelRepoName), Path(List("jar")), "file")
  }

  def replaced(m: MavenCoordinate, r: Replacements): Option[(Label, Language)] =
    r.get(m.unversioned).map { rr =>
      (Label.parse(rr.target.asString), rr.lang)
    }

  def localTarget(pathToRoot: List[String], m: MavenCoordinate, lang: Language): Label = {
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
  deps: List[Label],
  exports: List[Label],
  runtimeDeps: List[Label]) {

  def toBazelString: String = {
  /**
   * e.g.
   * scala_library(name = "foo",
   *   deps = [ ],
   *   exports = [ ],
   *   runtime_deps = [ ],
   *   visibility = ["//visibility:public"])
   */
    def labelList(outerIndent: Int, key: String, l: List[Label]): String = {
      if (l.isEmpty) ""
      else {
        val prefix = s"$key = ["
        val indent = prefix.length
        val spaced = List.fill(outerIndent + indent)(' ').mkString
        l.map { label =>
          val str = label.asStringFrom(name.path)
          s""""$str""""
        }.sorted.mkString("[", s",\n$spaced", "]")
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
