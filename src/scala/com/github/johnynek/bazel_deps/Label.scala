package com.github.johnynek.bazel_deps

import IO.Path

case class Label(workspace: Option[String], path: Path, name: String) {
  def packageLabel: Label = Label(workspace, path, "")

  private[this] val nmPart = if (name.isEmpty) "" else s":$name"

  def fromRoot: String = {
    val ws = workspace.fold("") { nm => s"@$nm" }
    path.parts match {
      case Nil =>
        if (nmPart.isEmpty) {
          if (ws.isEmpty) s"//" else s"$ws"
        }
        else s"$ws//$nmPart"
      case ps => ps.mkString(s"$ws//", "/", nmPart)
    }
  }

  def asStringFrom(p: Path): String = {
    if (workspace.isEmpty && path == p) nmPart
    else fromRoot
  }
}

object Label {
  def parse(str: String): Label = {
    val ws = if (str(0) == '@') Some(str.tail.takeWhile(_ != '/')) else None
    val wsLen = ws.fold(0)(_.length)
    val pathAndTarg = str.drop(1 + wsLen).dropWhile(_ == '/')
    val pathStr = pathAndTarg.takeWhile(_ != ':')

    val path =
      if (pathStr.isEmpty) Path(Nil)
      else Path(pathStr.split('/').toList)

    val target = pathAndTarg.drop(1 + pathStr.length)
    Label(ws, path, target)
  }
  def externalJar(lang: Language, u: UnversionedCoordinate, np: NamePrefix): Label = lang match {
    case Language.Java => Label(Some(u.toBazelRepoName(np)), Path(List("jar")), "")
    // If we know we have a scala jar, just use ":file" to be sure we can deal with macros
    case Language.Scala(_, _) => Label(Some(u.toBazelRepoName(np)), Path(List("jar")), "file")
    case Language.Kotlin => Label(Some(u.toBazelRepoName(np)), Path(List("jar")), "file")
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
      case Language.Java | Language.Kotlin => m.toTargetName
      case s@Language.Scala(_, true) => {
        val uvWithRemoved = s.removeSuffix(m)
        if (m == uvWithRemoved) {
          sys.error(s"scala coordinate: ${m.asString} does not have correct suffix for $s")
        } else {
          uvWithRemoved.toTargetName
        }
      }
      case Language.Scala(_, false) => m.toTargetName
    }

    val name = artName.map {
      case '.' => '_'
      case '-' => '_'
      case oth => oth
    }.mkString
    Label(None, p, name)
  }
}
