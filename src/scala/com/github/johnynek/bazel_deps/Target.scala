package com.github.johnynek.bazel_deps

object Target {
  sealed abstract class Kind(override val toString: String)
  case object Library extends Kind("library")
  case object Test extends Kind("test")
  case object Binary extends Kind("binary")

  sealed abstract class SourceList {
    def render(outerIndent: Int, key: String): String

  }
  object SourceList {
    case object Empty extends SourceList {
      def render(outerIndent: Int, key: String): String = ""
    }

    case class Explicit(srcs: Set[String]) extends SourceList {
      def render(outerIndent: Int, key: String): String =
        if (srcs.isEmpty) ""
        else {
          val prefix = s"$key = ["
          val indent = prefix.length
          val spaced = List.fill(outerIndent + indent)(' ').mkString
          srcs.toList.sorted.mkString("[", s",\n$spaced", "]")
        }
    }
    case class Globs(globs: List[String]) extends SourceList {
      def render(outerIndent: Int, key: String): String =
        if (globs.isEmpty) ""
        else {
          def quote(s: String): String = "\"%s\"".format(s)
          val gstr = globs.map(quote).mkString(", ")
          s"glob([$gstr])"
        }
    }
  }
}

case class Target(
  lang: Language,
  name: Label,
  kind: Target.Kind = Target.Library,
  deps: Set[Label] = Set.empty,
  sources: Target.SourceList = Target.SourceList.Empty,
  exports: Set[Label] = Set.empty,
  runtimeDeps: Set[Label] = Set.empty) {

  def toBazelString: String = {

    /**
     * e.g.
     * scala_library(name = "foo",
     *   deps = [ ],
     *   exports = [ ],
     *   runtime_deps = [ ],
     *   visibility = ["//visibility:public"])
     */
    def renderList[T](outerIndent: Int, key: String, l: Set[T])(show: T => String): String =
      if (l.isEmpty) ""
      else {
        val prefix = s"$key = ["
        val indent = prefix.length
        val spaced = List.fill(outerIndent + indent)(' ').mkString
        l.iterator.map(show).toList.sorted.mkString("[", s",\n$spaced", "]")
      }

    def labelList(outerIndent: Int, key: String, l: Set[Label]): String =
      renderList(outerIndent, key, l) { label =>
        val str = label.asStringFrom(name.path)
        "\"%s\"".format(str)
      }

    val langName = lang match {
      case Language.Java => "java"
      case Language.Scala(_, _) => "scala"
    }
    val header = s"${langName}_${kind}("
    def sortKeys(items: List[(String, String)]): String = {
      // everything has a name
      val nm = s"""name = "${name.name}""""
      if (items.isEmpty) { header + nm + ")" }
      else {
        val sorted = items.filter(_._2.nonEmpty).sortBy(_._1)
        val prefixIndent = List.fill(header.length)(' ').mkString
        (nm :: (sorted.map { case (k, v) => s"$prefixIndent$k = $v" }))
          .mkString(header, ",\n", ")")
      }
    }
    val indent = header.length
    sortKeys(List(
      "visibility" ->  """["//visibility:public"]""",
      "deps" -> labelList(indent, "deps", deps),
      "srcs" -> sources.render(indent, "srcs"),
      "exports" -> labelList(indent, "exports", exports),
      "runtime_deps" -> labelList(indent, "runtime_deps", runtimeDeps)))
  }
}
