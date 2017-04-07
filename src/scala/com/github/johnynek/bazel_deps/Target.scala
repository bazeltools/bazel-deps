package com.github.johnynek.bazel_deps

import com.github.johnynek.paiges.Doc

object Target {
  def renderList[T](front: Doc, l: List[T], back: Doc)(show: T => Doc): Doc =
    if (l.isEmpty) Doc.empty
    else {
      val spreadParts = Doc.intercalate(Doc.comma + Doc.line, l.map(show))
      front + (Doc.line + spreadParts).nest(4) + Doc.line + back
    }

  def quote(s: String): Doc =
    Doc.text("\"%s\"".format(s))

  sealed abstract class Kind(override val toString: String)
  case object Library extends Kind("library")
  case object Test extends Kind("test")
  case object Binary extends Kind("binary")

  sealed abstract class SourceList {
    def render: Doc

  }
  object SourceList {
    case object Empty extends SourceList {
      def render: Doc = Doc.empty
    }

    case class Explicit(srcs: Set[String]) extends SourceList {
      def render: Doc =
        if (srcs.isEmpty) Doc.empty
        else {
          renderList(Doc.text("["), srcs.toList.sorted, Doc.text("]"))(quote)
            .grouped
        }
    }
    case class Globs(globs: List[String]) extends SourceList {
      def render: Doc =
        if (globs.isEmpty) Doc.empty
        else {
          val gstr = renderList(Doc.text("["), globs, Doc.text("]"))(quote)
            .grouped
          Doc.text("glob(") + gstr + Doc.text(")")
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

  def toDoc: Doc = {
    import Target._
    /**
     * e.g.
     * scala_library(
     *     name = "foo",
     *     deps = [ ],
     *     exports = [ ],
     *     runtime_deps = [ ],
     *     visibility = ["//visibility:public"]
     * )
     */

    val langName = lang match {
      case Language.Java => "java"
      case Language.Scala(_, _) => "scala"
    }

    val targetType = Doc.text(s"${langName}_${kind}")

    def sortKeys(items: List[(String, Doc)]): Doc = {
      // everything has a name
      val nm = ("name", quote(name.name))
      val sorted = items.collect { case (s, d) if !(d.isEmpty) => (s, d) }.sorted

      renderList(targetType + Doc.text("("), nm :: sorted, Doc.text(")")) { case (k, v) =>
        k +: " = " +: v
      } + Doc.line.repeat(2)
    }

    def labelList(ls: Set[Label]): Doc =
      renderList(Doc.text("["), ls.toList.map(_.asStringFrom(name.path)).sorted, Doc.text("]"))(quote)

    sortKeys(List(
      "visibility" -> renderList(Doc.text("["), List("//visibility:public"), Doc.text("]"))(quote),
      "deps" -> labelList(deps),
      "srcs" -> sources.render,
      "exports" -> labelList(exports),
      "runtime_deps" -> labelList(runtimeDeps)))
  }
}
