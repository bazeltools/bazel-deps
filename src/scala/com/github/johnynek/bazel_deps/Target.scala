package com.github.johnynek.bazel_deps

import cats.Show
import IO.Path

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
  runtimeDeps: Set[Label])

object Target {
  def labelList0(outerIndent: Int, key: String, l: Set[String]): String = {
    if (l.isEmpty) ""
    else {
      val prefix = s"$key = ["
      val indent = prefix.length
      val spaced = List.fill(outerIndent + indent)(' ').mkString
      l.map { str =>
        s""""$str""""
      }.toList.sorted.mkString("[", s",\n$spaced", "]")
    }
  }
  def sortKeys0(header: String, name: String, items: List[(String, String)]): String = {
    // everything has a name
    val nm = s"""name = "$name""""
    if (items.isEmpty) { header + nm + ")" }
    else {
      val sorted = items.sortBy(_._1).filter(_._2.nonEmpty)
      val prefixIndent = List.fill(header.length)(' ').mkString
      (nm :: (sorted.map { case (k, v) => s"$prefixIndent$k = $v" }))
        .mkString(header, ",\n", ")")
    }
  }

  def legacyShow: Show[Target] =
    show(
      { case (h, n, its) => sortKeys0(h, n, its) },
      { case (o, k, vs) => labelList0(o, k, vs) }
    )

  def show(
    item: ((String, String, List[(String, String)])) => String,
    list: ((Int, String, Set[String]) => String)): Show[Target] = Show.show { t: Target =>
    import t._
    /**
     * e.g.
     * scala_library(name = "foo",
     *   deps = [ ],
     *   exports = [ ],
     *   runtime_deps = [ ],
     *   visibility = ["//visibility:public"])
     */
      val langName = lang match {
        case Language.Java => "java"
        case Language.Scala(_, _) => "scala"
      }
      val header = s"${langName}_library("
      val indent = header.length
      def from(s: Set[Label]) = s.map(_.asStringFrom(name.path))
      item(header, name.name, List(
        "visibility" ->  """["//visibility:public"]""",
        "deps" -> list(indent, "deps", from(deps)),
        "exports" -> list(indent, "exports", from(exports)),
        "runtime_deps" -> list(indent, "runtime_deps", from(runtimeDeps))))
    }
}
