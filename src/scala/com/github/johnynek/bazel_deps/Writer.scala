package com.github.johnynek.bazel_deps

object Writer {
  def workspace(g: Graph[MavenCoordinate, Unit]): String = {
    val nodes = g.nodes
    val lines = nodes.map { case coord@MavenCoordinate(g, a, v) =>
        s"""  callback({"name": "${coord.toBazelRepoName}", "artifact": "${coord.asString}"})"""
      }
      .mkString("\n")
    s"""def maven_dependencies(callback):\n$lines"""
  }

  def targets(g: Graph[MavenCoordinate, Unit]): List[Target] = {
    /*
     * We make 1 label for each target, the path
     * and name are derived from the MavenCoordinate
     */
    val cache = scala.collection.mutable.Map[MavenCoordinate, Target]()
    def coordToTarget(c: MavenCoordinate): Target = cache.getOrElseUpdate(c, {
      val deps = g.hasSource(c)
      val depLabels = deps.map { e => coordToTarget(e.destination).name }.toList
      val exports = List(Label.externalJar(c))
      Target(Language.Java, Label.localTarget(c), depLabels, exports, Nil)
    })

    g.nodes.iterator.map(coordToTarget).toList
  }
}

case class Path(parts: List[String])
case class Label(workspace: Option[String], path: Path, name: String) {
  def asStringFrom(p: Path): String =
    if (workspace.isEmpty && path == p) s":$name"
    else {
      val ws = workspace.fold("") { nm => s"@$nm" }
      path.parts match {
        case Nil => s"$ws//:$name"
        case ps => ps.mkString(s"$ws//", "/", s":$name")
      }
    }
}

object Label {
  def externalJar(m: MavenCoordinate): Label =
    Label(Some(m.toBazelRepoName), Path(Nil), "jar")

  def localTarget(m: MavenCoordinate): Label = {
    val p = Path(m.group.asString.map {
      case '.' => '/'
      case '-' => '_'
      case other => other
    }.mkString
    .split('/')
    .toList)

    val name = m.artifact.asString.map {
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
    val header = "java_library("
    def sortKeys(items: List[(String, String)]): String = {
      // everything has a name
      val nm = s"""name = "${name.name}"""
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
