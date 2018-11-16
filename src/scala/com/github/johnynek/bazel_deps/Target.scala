package com.github.johnynek.bazel_deps

import org.typelevel.paiges.Doc

object Target {
  def renderList[T](front: Doc, l: List[T], back: Doc)(show: T => Doc): Doc =
    if (l.isEmpty) Doc.empty
    else {
      val spreadParts = Doc.intercalate(Doc.comma + Doc.line, l.map(show))
      front + (Doc.line + spreadParts).nested(4) + Doc.line + back
    }

  def quote(s: String): Doc =
    Doc.text("\"%s\"".format(s))

  def bool(b: Boolean): Doc =
    Doc.text("%b".format(b).capitalize)

  def fqnToLabelFragment(fqn: String): String =
    fqn.toLowerCase.replaceAll("[^a-z0-9]", "_")

  sealed abstract class Kind(override val toString: String)
  case object Library extends Kind("library")
  case object Import extends Kind("import")
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

  sealed abstract class Visibility(val asString: String)
  object Visibility {
    case object Public extends Visibility("//visibility:public")
    case class SubPackages(of: Label) extends Visibility(s"${of.packageLabel.fromRoot}:__subpackages__")
  }
}

case class Target(
  lang: Language,
  name: Label,
  visibility: Target.Visibility,
  kind: Target.Kind = Target.Library,
  deps: Set[Label] = Set.empty,
  jars: Set[Label] = Set.empty,
  sources: Target.SourceList = Target.SourceList.Empty,
  exports: Set[Label] = Set.empty,
  runtimeDeps: Set[Label] = Set.empty,
  processorClasses: Set[ProcessorClass] = Set.empty,
  generatesApi: Boolean = false,
  licenses: Set[String] = Set.empty) {

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
      case Language.Kotlin => "kt_jvm"
      case Language.Scala(_, _) => "scala"
    }

    val targetType = Doc.text(s"${langName}_${kind}")

    def sortKeys(tt: Doc, name: String, items: List[(String, Doc)]): Doc = {
      // everything has a name
      val nm = ("name", quote(name))
      implicit val ordDoc: Ordering[Doc] = Ordering.by { d: Doc => d.renderWideStream.mkString }
      val sorted = items.collect { case (s, d) if !(d.isEmpty) => (s, d) }.sorted

      renderList(tt + Doc.text("("), nm :: sorted, Doc.text(")")) { case (k, v) =>
        k +: " = " +: v
      } + Doc.line
    }

    def labelList(ls: Set[Label]): Doc =
      renderList(Doc.text("["), ls.toList.map(_.asStringFrom(name.path)).sorted, Doc.text("]"))(quote)

    def renderExportedPlugins(pcs: Set[ProcessorClass]): Doc =
      renderList(Doc.text("["), pcs.toList.map(pc => ":" + getPluginTargetName(pcs, pc)).sorted, Doc.text("]"))(quote)

    def getPluginTargetName(pcs: Set[ProcessorClass], pc: ProcessorClass) =
      if (pcs.size == 1) s"${name.name}_plugin"
      else s"${name.name}_plugin_${fqnToLabelFragment(pc.asString)}"

    def renderPlugins(pcs: Set[ProcessorClass], exports: Set[Label], generatesApi: Boolean, licenses: Set[String]): Doc =
      if (pcs.isEmpty) Doc.empty
      else processorClasses.toList.sortBy(_.asString).map(renderPlugin(pcs, _, exports, generatesApi, licenses)).reduce((d1, d2) => d1 + d2)

    def renderPlugin(pcs: Set[ProcessorClass], pc: ProcessorClass,exports: Set[Label], generatesApi: Boolean, licenses: Set[String]): Doc =
      sortKeys(Doc.text("java_plugin"), getPluginTargetName(pcs, pc), List(
        "deps" -> labelList(exports ++ jars ++ deps ++ runtimeDeps),
        "licenses" -> renderLicenses(licenses),
        "generates_api" -> bool(generatesApi),
        "processor_class" -> quote(pc.asString),
        visibilityDoc
      )) + Doc.line

    def visibilityDoc: (String, Doc) =
      "visibility" -> renderList(Doc.text("["), List(visibility.asString), Doc.text("]"))(quote)

    def renderLicenses(licenses: Set[String]): Doc =
      if (!licenses.isEmpty) renderList(Doc.text("["), licenses.toList, Doc.text("]"))(quote)
      else Doc.empty

    sortKeys(targetType, name.name, List(
      visibilityDoc,
      "deps" -> labelList(deps),
      "licenses" -> renderLicenses(licenses),
      "srcs" -> sources.render,
      "jars" -> labelList(jars),
      "exports" -> labelList(exports),
      "runtime_deps" -> labelList(runtimeDeps),
      "exported_plugins" -> renderExportedPlugins(processorClasses)
    )) + renderPlugins(processorClasses, exports, generatesApi, licenses) + Doc.line
  }
}
