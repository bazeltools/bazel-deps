package com.github.johnynek.bazel_deps

import java.io.{ File, FileOutputStream }

object Writer {

  def createBuildFiles(pathToRoot: File, ts: List[Target]): Unit = {
    require(pathToRoot.isAbsolute, s"Absolute path required, found: $pathToRoot")

    def fileFor(p: Path): File =
      p.parts.foldLeft(pathToRoot) { (p, element) => new File(p, element) }

    ts.groupBy(_.name.path)
      .foreach { case (path, ts) =>
        val filePath = fileFor(path)
        require(filePath.exists || filePath.mkdirs(), s"failed to create $filePath")
        val buildFile = new File(filePath, "BUILD")
        val os = new FileOutputStream(buildFile)
        try {
          val fileBytes = ts.sortBy(_.name.name)
            .map(_.toBazelString)
            .mkString("\n\n")
            .getBytes("UTF-8")

          os.write(fileBytes)
        } finally {
          os.close()
        }
      }
  }

  def workspace(g: Graph[MavenCoordinate, Unit], model: Model): String = {
    val nodes = g.nodes

    def replaced(m: MavenCoordinate): Boolean = model.getReplacements.get(m.unversioned).isDefined

    val lines = nodes.filterNot(replaced)
      .toList
      .sortBy(_.asString)
      .map { case coord@MavenCoordinate(g, a, v) =>
        s"""  callback({ "artifact": "${coord.asString}", "name": "${coord.toBazelRepoName}" })"""
      }
      .mkString("\n")
    s"""def maven_dependencies(callback):\n$lines"""
  }

  def targets(g: Graph[MavenCoordinate, Unit],
    pathInRoot: List[String],
    model: Model): List[Target] = {
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
            (Label.externalJar(c), model.languageOf(c))
          }
      val exports = lab :: depLabels
      Target(lang, Label.localTarget(pathInRoot, c, lang), Nil, exports, Nil)
    })

    g.nodes.iterator.map(coordToTarget).toList
  }
}

case class Path(parts: List[String])
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
  def externalJar(m: MavenCoordinate): Label =
    Label(Some(m.toBazelRepoName), Path(List("jar")), "")

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
      case s@Language.Scala(_) =>
        s.removeSuffix(m.artifact.asString) match {
          case Some(n) => n
          case None => sys.error(s"scala coordinate: ${m.asString} does not have correct suffix for $s")
        }
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
    def labelList(key: String, l: List[Label]): String = {
      if (l.isEmpty) ""
      else {
        val prefix = s"    $key = ["
        val indent = prefix.length
        val spaced = List.fill(indent)(' ').mkString
        l.map { label =>
          val str = label.asStringFrom(name.path)
          s""""$str""""
        }.sorted.mkString("[", s",\n$spaced", "]")
      }
    }
    val langName = lang match {
      case Language.Java => "java"
      case Language.Scala(_) => "scala"
    }
    val header = s"${langName}_library("
    def sortKeys(items: List[(String, String)]): String = {
      // everything has a name
      val nm = s"""name = "${name.name}""""
      if (items.isEmpty) { header + nm + ")" }
      else {
        val sorted = items.sortBy(_._1).filter(_._2.nonEmpty)
        (nm :: (sorted.map { case (k, v) => s"    $k = $v" }))
          .mkString(header, ",\n", ")")
      }
    }

    sortKeys(List(
      "visibility" ->  """["//visibility:public"]""",
      "deps" -> labelList("deps", deps),
      "exports" -> labelList("exports", exports),
      "runtime_deps" -> labelList("runtime_deps", runtimeDeps)))
  }
}
