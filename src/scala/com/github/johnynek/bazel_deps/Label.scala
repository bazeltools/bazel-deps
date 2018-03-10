package com.github.johnynek.bazel_deps

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
  def externalJar(lang: Language, u: UnversionedCoordinate, np: NamePrefix): Label = lang match {
    case Language.Java => Label(Some(u.toBazelRepoName(np)), Path(List("jar")), "")
    // If we know we have a scala jar, just use ":file" to be sure we can deal with macros
    case Language.Scala(_, _) => Label(Some(u.toBazelRepoName(np)), Path(List("jar")), "file")
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
      case ':' => '_'
      case oth => oth
    }.mkString
    Label(None, p, name)
  }
}
