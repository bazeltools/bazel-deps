package com.github.johnynek.bazel_deps

import java.util.regex.Pattern

import cats.Traverse
import com.github.johnynek.bazel_deps.IO.Result
import org.typelevel.paiges.Doc
import cats.implicits._

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
  object Kind {
    def parse(str: String): Result[Kind] = str match {
      case "library" => IO.const(Library)
      case "import" => IO.const(Import)
      case "test" => IO.const(Test)
      case "binary" => IO.const(Binary)
      case other => IO.failed(new IllegalArgumentException(s"unexpected library kind: $other"))
    }
  }
  case object Library extends Kind("library")
  case object Import extends Kind("import")
  case object Test extends Kind("test")
  case object Binary extends Kind("binary")

  sealed abstract class SourceList {
    def render: Doc
    def asStringList: List[String]
  }
  object SourceList {
    def parseStringList(l: List[String]): Result[SourceList] = l match {
      case Nil => IO.const(SourceList.Empty)
      case "E" :: t => IO.const(SourceList.Explicit(t.toSet))
      case "G" :: t => IO.const(SourceList.Globs(t))
      case o => IO.failed(new Exception(s"Unable to parse $o as a Source list."))
    }

    case object Empty extends SourceList {
      def render: Doc = Doc.empty
      def asStringList: List[String] = Nil
    }

    case class Explicit(srcs: Set[String]) extends SourceList {
      def render: Doc =
        if (srcs.isEmpty) Doc.empty
        else {
          renderList(Doc.text("["), srcs.toList.sorted, Doc.text("]"))(quote)
            .grouped
        }

      def asStringList: List[String] = "E" :: srcs.toList
    }
    case class Globs(globs: List[String]) extends SourceList {
      def asStringList: List[String] = "G" :: globs.toList

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
    def parse(str: String): Result[Visibility] =
      str match {
        case "//visibility:public" => IO.const(Public)
        case e if e.endsWith(":__subpackages__") => IO.const(SubPackages(Label.parse(e.dropRight(":__subpackages__".size))))
        case o => IO.failed(new Exception(s"Unable to parse visibility: $o"))
      }
  }

  private[this] def parseLanguage(language: String): Result[Language] = language match {
    case "kotlin" => IO.const(Language.Kotlin)
    case "java" => IO.const(Language.Java)
    case e if e.startsWith("scala") =>
      e.split(":").toList match {
        case "scala" :: v :: Nil => IO.const(Language.Scala(Version.apply(v), true))
        case "scala/unmangled" :: v :: Nil => IO.const(Language.Scala(Version.apply(v), true))
        case o => IO.failed(new Exception(s"Unable to parse scala configuration string: $e"))
      }
    case o => IO.failed(new Exception(s"Unable to parse language for $o"))
  }

  def fromListStringEncoding(rawSep: String, encodedContent: List[String]): Result[Target] = {
    val seperator = Pattern.quote(rawSep)
    val resultV: Result[Map[String, List[String]]] = Traverse[List].traverse(encodedContent) { ln: String =>
      val res: Result[(String, List[String])] = ln.split(seperator).toList match {
        case Nil => IO.failed(new Exception("Got empty content"))
        case h :: "" :: e :: Nil => IO.const((h, List(e)))
        case h :: "B" :: e :: Nil => IO.const((h, List(e)))
        case h :: "L" :: t => IO.const((h, t))
        case o => IO.failed(new Exception(s"Unable to parse passed input: $o"))
      }
      res
    }.map(_.toMap)
    resultV.flatMap { entries =>
      def get(name: String): Result[List[String]] =
        entries.get(name) match {
          case Some(e) => IO.const(e)
          case None => IO.failed(new Exception(s"Unable to find $name in input map, likely invalid data"))
        }


      def optionToResult[T](opt: Option[T]): Result[T] = opt match {
        case Some(e) => IO.const(e)
        case None => IO.failed(new Exception("Error accessing empty option"))
      }

      def getS(name: String): Result[String] =
        get(name).flatMap(e => optionToResult(e.headOption))

      def getBoolean(name: String): Result[Boolean] =
        getS(name).flatMap {
          case "true" => IO.const(true)
          case "false" => IO.const(false)
          case o => IO.failed(new Exception(s"unable to parse boolean as value $o"))
        }

      for {
          rawLang <- getS("lang")
          lang <- parseLanguage(rawLang)
          rawName <- getS("name")
          name = Label.parse(rawName)
          visibility <- getS("visibility").flatMap(Visibility.parse)
          kind <- getS("kind").flatMap(Kind.parse)
          deps <- get("deps").map(_.map(Label.parse).toSet)
          jars <- get("jars").map(_.map(Label.parse).toSet)
          sources <-  get("sources").flatMap(SourceList.parseStringList)
          exports <- get("exports").map(_.map(Label.parse).toSet)
          runtimeDeps <- get("runtimeDeps").map(_.map(Label.parse).toSet)
          processorClasses <- get("processorClasses").map(_.map(ProcessorClass).toSet)
          licenses <- get("licenses").map(_.toSet)
          generatesApi <- getBoolean("generatesApi")
          generateNeverlink <- getBoolean("generateNeverlink")
      } yield {
        Target(
          lang, name, visibility, kind, deps, jars, sources, exports, runtimeDeps, processorClasses, generatesApi, licenses, generateNeverlink)
      }
    }
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
  licenses: Set[String] = Set.empty,
  generateNeverlink: Boolean = false) {

  /**
    * This method is for encoding the target such that it can be represented in bazel as a list
    * of strings.
    * Across the repository rule boundary we cannot pass complex types and we don't want an external binary
    * to be required to do parsing/handling of a better format.
    *
    * Spec: <name>|||<type>|||<values>
    * Where ||| is standing in for the separator passed in
    * Name is the field name as specified in the list below
    * Type is L for list of elements, B for a boolean or empty for a normal string type
    * Values are 0 or more strings that have no separator field in them separated by the separator.
    *
    * @param separator this is the entry separator we've been passed in.
    * @return List of entries that describe this target encoded
    */
  def listStringEncoding(separator: String): Result[List[String]] = {
    def validate(strV: String): Result[Unit] =
      if(strV.contains("|")) {
        IO.failed(new Exception(s"Unable to encode ${strV} contains a | which isn't supported for bzl file encoding."))
      } else IO.unit

    def withName(name: String, v: String): Result[String] =
      validate(v).map {_ => s"${name}${separator}${separator}$v"}

    def withNameL(name: String, v: Iterable[String]): Result[String] =
      Traverse[List].traverse(v.toList) { e =>
        validate(e)
      }.map { _ =>
        val strV = v.mkString(separator)
        s"${name}${separator}L${separator}$strV"
      }

    def withNameB(name: String, v: Boolean): Result[String] =
      IO.const(s"${name}${separator}B${separator}$v")

    Traverse[List].sequence(List[Result[String]](
      withName("lang", lang.asReversableString),
      withName("name", name.fromRoot),
      withName("visibility", visibility.asString),
      withName("kind", kind.toString),
      withNameL("deps", deps.map(_.fromRoot)),
      withNameL("jars", jars.map(_.fromRoot)),
      withNameL("sources", sources.asStringList),
      withNameL("exports", exports.map(_.fromRoot)),
      withNameL("runtimeDeps", runtimeDeps.map(_.fromRoot)),
      withNameL("processorClasses", processorClasses.map(_.asString)),
      withNameB("generatesApi", generatesApi),
      withNameL("licenses", licenses),
      withNameB("generateNeverlink", generateNeverlink)
    ))
  }

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

    def generateTarget(neverlink: Boolean): Doc = {
      val defaultArgs = List(
        visibilityDoc,
        "deps" -> labelList(deps),
        "licenses" -> renderLicenses(licenses),
        "srcs" -> sources.render,
        "jars" -> labelList(jars),
        "exports" -> labelList(exports),
        "runtime_deps" -> labelList(runtimeDeps),
        "exported_plugins" -> renderExportedPlugins(processorClasses)
      )
      val (targetName, targetArgs) =
        if (neverlink) (name.name + "_neverlink", defaultArgs :+ (("neverlink", Doc.text("1"))))
        else (name.name, defaultArgs)

      sortKeys(targetType, targetName, targetArgs) + renderPlugins(processorClasses, exports, generatesApi, licenses) + Doc.line
    }

    if (!generateNeverlink) {
      generateTarget(neverlink = false)
    } else {
      generateTarget(neverlink = false) + generateTarget(neverlink = true)
    }
  }
}
