package com.github.johnynek.bazel_deps

import java.io.{ File, BufferedReader, FileReader }
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

import com.github.johnynek.paiges.Doc

/**
 * These should be upstreamed to paiges
 */
object DocUtil {
  def packedKV(k: String, v: Doc): Doc =
    Doc.text(k) +: Doc.text(":") +: Doc.spaceOrLine.nest(2) +: v

  def kv(k: String, v: Doc, tight: Boolean = false): Doc =
    Doc.text(k) +: Doc.text(":") +: ((Doc.line +: v).nest(2))
  def quote(s: String): String = "\"%s\"".format(s)
  def quoteDoc(s: String): Doc = Doc.text(quote(s))

  def list[T](i: Iterable[T])(show: T => Doc): Doc = {
    val parts = Doc.intercalate(Doc.comma, i.map { j => (Doc.line +: show(j)).group })
    "[" +: (parts :+ " ]").nest(2)
  }
  // Here is a vertical list of docs
  def vlist(ds: Iterable[Doc]): Doc = {
    val dash = Doc.text("- ")
    Doc.intercalate(Doc.line, ds.map { d => dash +: d.nest(2) })
  }

  def yamlMap(kvs: List[(String, Doc)], lines: Int = 1): Doc = {
    def rep(x: Int): Doc =
      if (x <= 0) Doc.empty
      else Doc.line +: rep(x - 1)

    if (kvs.isEmpty) Doc.text("{}")
    else Doc.intercalate(rep(lines), kvs.map { case (k, v) => kv(k, v) })
  }

  def packedYamlMap(kvs: List[(String, Doc)]): Doc =
    if (kvs.isEmpty) Doc.text("{}")
    else Doc.intercalate(Doc.line, kvs.map { case (k, v) => packedKV(k, v) })
}

import DocUtil._

case class Model(
  dependencies: Dependencies,
  replacements: Option[Replacements],
  options: Option[Options]) {

  def getOptions: Options =
    options.getOrElse(Options.default)

  def getReplacements: Replacements =
    replacements.getOrElse(Replacements.empty)

  def toDoc: Doc = {
    val deps = Some(("dependencies", dependencies.toDoc))
    val reps = replacements.map { r => ("replacements", r.toDoc) }
    val opts = options.map { o => ("options", o.toDoc) }

    yamlMap(List(opts, deps, reps).collect { case Some(kv) => kv }, 2)
  }
}

object Model {
  def readFile(f: File): Try[String] = Try {
    val fr = new FileReader(f)
    try {
      val buf = new BufferedReader(fr)
      val bldr = new java.lang.StringBuilder
      val cbuf = new Array[Char](1024)
      var read = 0
      while(read >= 0) {
        read = buf.read(cbuf, 0, 1024)
        if (read > 0) bldr.append(cbuf, 0, read)
      }
      Success(bldr.toString)
    }
    catch {
      case NonFatal(err) => Failure(err)
    }
    finally {
      fr.close
    }
  }.flatten
}

case class MavenGroup(asString: String)
case class ArtifactOrProject(asString: String) {
  def splitSubprojects: List[(ArtifactOrProject, Subproject)] =
    if (asString.contains('-')) {
      val parts = asString.split('-').toList
      (1 until parts.size).map { i =>
        val (a, s) = parts.splitAt(i)
        (ArtifactOrProject(a.mkString("-")), Subproject(s.mkString("-")))
      }.toList
    }
    else Nil
}
case class Subproject(asString: String)
case class Version(asString: String)
case class Sha1Value(toHex: String)
case class MavenServer(id: String, contentType: String, url: String) {
  def toDoc: Doc =
    packedYamlMap(
      List(("id", quoteDoc(id)), ("type", quoteDoc(contentType)), ("url", Doc.text(url))))
}

object Version {
  private def isNum(c: Char): Boolean =
    ('0' <= c) && (c <= '9')
  /**
   * break a string into alternating runs of Longs and Strings
   */
  private def tokenize(s: String): List[Either[String, Long]] = {
    def append(a: List[Either[String, Long]], b: Either[List[Char], List[Char]]): List[Either[String, Long]] =
      b match {
        case Right(thisAcc) =>
          Right(thisAcc.reverse.mkString.toLong) :: a
        case Left(thisAcc) =>
          Left(thisAcc.reverse.mkString) :: a
      }

    val (acc, toAdd) =
      s.foldLeft((List.empty[Either[String, Long]], Option.empty[Either[List[Char], List[Char]]])) {
        // Here are the first characters
        case ((acc, None), c) if isNum(c) =>
          (acc, Some(Right(c :: Nil)))
        case ((acc, None), c) if !isNum(c) =>
          (acc, Some(Left(c :: Nil)))
        // Here we continue with the same type
        case ((acc, Some(Right(thisAcc))), c) if isNum(c) =>
          (acc, Some(Right(c :: thisAcc)))
        case ((acc, Some(Left(thisAcc))), c) if !isNum(c)=>
          (acc, Some(Left(c :: thisAcc)))
        // Here we switch type and add to the acc
        case ((acc, Some(r@Right(thisAcc))), c) if !isNum(c)=>
          (append(acc, r), Some(Left(c :: Nil)))
        case ((acc, Some(l@Left(thisAcc))), c) if isNum(c) =>
          (append(acc, l), Some(Right(c :: Nil)))
      }
    toAdd.fold(acc)(append(acc, _)).reverse
  }

  implicit def versionOrdering: Ordering[Version] = {
    implicit val strNumOrd: Ordering[Either[String, Long]] = new Ordering[Either[String, Long]] {
      def compare(left: Either[String, Long], right: Either[String, Long]): Int = {
        (left, right) match {
          case (Right(a), Right(b)) => java.lang.Long.compare(a, b)
          case (Right(_), Left(_)) => 1 // put non number before number (eg, "-RC" comes before 2)
          case (Left(_), Right(_)) => -1
          case (Left(a), Left(b)) => a.compareTo(b)
            val commonTokens = Set("alpha", "beta", "pre", "rc", "m")
            val al = a.toLowerCase
            val bl = b.toLowerCase
            if (commonTokens(al) && commonTokens(bl)) {
              al.compareTo(bl)
            } else a.compareTo(b)
        }
      }
    }
    // In versions, if one is a prefix of the other, and the next item is
    // not a number, it is bigger.
    @annotation.tailrec
    def prefixCompare[T: Ordering](a: List[T], b: List[T])(fn: T => Int): Int = (a, b) match {
      case (Nil, h :: tail) => fn(h)
      case (h :: tail, Nil) => -fn(h)
      case (Nil, Nil) => 0
      case (ha :: taila, hb :: tailb) =>
        val c = Ordering[T].compare(ha, hb)
        if (c == 0) prefixCompare(taila, tailb)(fn)
        else c
    }
    Ordering.by { v: Version =>
      v.asString.split("\\.|\\-") // note this is a regex
        .flatMap(tokenize)
        .toList
    }(new Ordering[List[Either[String, Long]]] {
      def compare(a: List[Either[String, Long]], b: List[Either[String, Long]]) =
        prefixCompare(a, b) {
          case Left(_) => 1 // if see a string, the shorter one is larger
          case Right(_) => -1 // if we see a number, the shorter is smaller
        }
    })
  }
}

case class MavenArtifactId(asString: String) {
  def addSuffix(s: String): MavenArtifactId = MavenArtifactId(asString + s)
}

object MavenArtifactId {
  def apply(a: ArtifactOrProject): MavenArtifactId = MavenArtifactId(a.asString)
  def apply(a: ArtifactOrProject, s: Subproject): MavenArtifactId = {
    val ap = a.asString
    val sp = s.asString
    MavenArtifactId(if (sp.isEmpty) ap else s"$ap-$sp")
  }
}

case class MavenCoordinate(group: MavenGroup, artifact: MavenArtifactId, version: Version) {
  def unversioned: UnversionedCoordinate = UnversionedCoordinate(group, artifact)
  def asString: String = s"${group.asString}:${artifact.asString}:${version.asString}"
}

object MavenCoordinate {
  def apply(s: String): MavenCoordinate = {
    s.split(":") match {
      case Array(g, a, v) => MavenCoordinate(MavenGroup(g), MavenArtifactId(a), Version(v))
      case other => sys.error(s"expected exactly three :, got $s")
    }
  }
  def apply(u: UnversionedCoordinate, v: Version): MavenCoordinate =
    MavenCoordinate(u.group, u.artifact, v)

  implicit def mvnCoordOrd: Ordering[MavenCoordinate] = Ordering.by { m: MavenCoordinate =>
    (m.group.asString, m.artifact.asString, m.version)
  }
}

sealed abstract class Language {
  def asString: String
  def asOptionsString: String
  def mavenCoord(g: MavenGroup, a: ArtifactOrProject, v: Version): MavenCoordinate
  def mavenCoord(g: MavenGroup, a: ArtifactOrProject, sp: Subproject, v: Version): MavenCoordinate
  def unversioned(g: MavenGroup, a: ArtifactOrProject): UnversionedCoordinate
  def unversioned(g: MavenGroup, a: ArtifactOrProject, sp: Subproject): UnversionedCoordinate
}

object Language {
  case object Java extends Language {
    def asString = "java"
    def asOptionsString = asString
    def mavenCoord(g: MavenGroup, a: ArtifactOrProject, v: Version): MavenCoordinate =
      MavenCoordinate(g, MavenArtifactId(a), v)

    def mavenCoord(g: MavenGroup, a: ArtifactOrProject, sp: Subproject, v: Version): MavenCoordinate =
      MavenCoordinate(g, MavenArtifactId(a, sp), v)

    def unversioned(g: MavenGroup, a: ArtifactOrProject): UnversionedCoordinate =
      UnversionedCoordinate(g, MavenArtifactId(a))

    def unversioned(g: MavenGroup, a: ArtifactOrProject, sp: Subproject): UnversionedCoordinate =
      UnversionedCoordinate(g, MavenArtifactId(a, sp))
  }

  case class Scala(v: Version, mangle: Boolean) extends Language {
    def asString = if (mangle) "scala" else "scala/unmangled"
    def asOptionsString: String = s"scala:${v.asString}"

    val major = v.asString.split('.') match {
      case Array("2", x) if (x.toInt >= 10) => s"2.$x"
      case Array("2", x, _) if (x.toInt >= 10) => s"2.$x"
      case _ => sys.error(s"unsupported scala version: ${v.asString}")
    }
    private val suffix = s"_$major"
    private def add(a: MavenArtifactId): MavenArtifactId =
      if (mangle) a.addSuffix(suffix)
      else a

    def unversioned(g: MavenGroup, a: ArtifactOrProject): UnversionedCoordinate =
      UnversionedCoordinate(g, add(MavenArtifactId(a)))

    def unversioned(g: MavenGroup, a: ArtifactOrProject, sp: Subproject): UnversionedCoordinate =
      UnversionedCoordinate(g, add(MavenArtifactId(a, sp)))

    def mavenCoord(g: MavenGroup, a: ArtifactOrProject, v: Version): MavenCoordinate =
      MavenCoordinate(g, add(MavenArtifactId(a)), v)

    def mavenCoord(g: MavenGroup, a: ArtifactOrProject, sp: Subproject, v: Version): MavenCoordinate =
      MavenCoordinate(g, add(MavenArtifactId(a, sp)), v)

    def removeSuffix(s: String): Option[String] =
      if (s.endsWith(suffix)) Some(s.dropRight(suffix.size))
      else None

    def endsWithScalaVersion(uv: UnversionedCoordinate): Boolean =
      uv.asString.endsWith(suffix)
  }
}

case class UnversionedCoordinate(group: MavenGroup, artifact: MavenArtifactId) {
  def asString: String = s"${group.asString}:${artifact.asString}"
  /**
   * This is a bazel-safe name to use as a remote repo name
   */
  def toBazelRepoName: String =
    asString.map {
      case '.' => "_"  // todo, we should have something such that if a != b this can't be equal, but this can
      case '-' => "_"
      case ':' => "_"
      case other => other
    }
    .mkString

  def toBindingName: String = {
    val g = group.asString.map {
      case '.' => '/'
      case o => o
    }
    s"jar/$g/${artifact.asString}".map {
      case '.' | '-' => '_'
      case o => o
    }
  }
  def bindTarget: String = s"//external:$toBindingName"
}

case class ProjectRecord(
  lang: Language,
  version: Option[Version],
  modules: Option[List[Subproject]],
  exports: Option[List[(MavenGroup, ArtifactOrProject)]],
  exclude: Option[List[(MavenGroup, ArtifactOrProject)]]) {

  def withModule(m: Subproject): ProjectRecord = modules match {
    case None =>
      copy(modules = Some(List(m)))
    case Some(subs) =>
      // we need to put "m-" on the front of everything
      val newMod = Some(subs.map { sp => Subproject(s"${m.asString}-${sp.asString}") })
      copy(modules = newMod)
  }

  def combineModules(that: ProjectRecord): Option[ProjectRecord] =
    if ((lang == that.lang) &&
        (version.flatMap { v => that.version.map(_ == v) }.forall(_ == true)) &&
        (exports == that.exports) &&
        (exclude == that.exclude)) {
      val mods = (modules, that.modules) match {
        case (Some(a), Some(b)) => Some(a ++ b)
        case (None, s) => s
        case (s, None) => s
      }

      Some(copy(modules = mods))
    } else None

  def getModules: List[Subproject] = modules.getOrElse(Nil)

  def versionedDependencies(g: MavenGroup,
    ap: ArtifactOrProject): List[MavenCoordinate] =
    version.fold(List.empty[MavenCoordinate]) { v =>
      getModules match {
        case Nil => List(lang.mavenCoord(g, ap, v))
        case mods => mods.map { m => lang.mavenCoord(g, ap, m, v) }
      }
    }

  def allDependencies(g: MavenGroup, ap: ArtifactOrProject): List[UnversionedCoordinate] =
    getModules match {
      case Nil => List(lang.unversioned(g, ap))
      case mods => mods.map { m => lang.unversioned(g, ap, m) }
    }

  def toDoc: Doc = {
    def colonPair(a: MavenGroup, b: ArtifactOrProject): Doc =
      quoteDoc(s"${a.asString}:${b.asString}")

    val record = List(List(("lang", Doc.text(lang.asString))),
      version.toList.map { v => ("version", quoteDoc(v.asString)) },
      modules.toList.map { ms => ("modules", list(ms) { m => quoteDoc(m.asString) }) },
      exports.toList.map { ms => ("exports", list(ms) { case (a, b) => colonPair(a, b) }) },
      exclude.toList.map { ms => ("exclude", list(ms) { case (a, b) => colonPair(a, b)}) })
      .flatten
      .sortBy(_._1)
    packedYamlMap(record)
  }
}

case class Dependencies(toMap: Map[MavenGroup, Map[ArtifactOrProject, ProjectRecord]]) {

  def toDoc: Doc = {
    type Pair = (ArtifactOrProject, ProjectRecord)
    def merge(a: Pair, b: Pair): Option[Pair] = {
      def subs(p: Pair): List[Pair] =
        p._1.splitSubprojects match {
          case Nil => List(p)
          case sub => sub.map { case (a, s) => (a, p._2.withModule(s)) }
        }

      val merges = for {
        (aa, pra) <- subs(a)
        (ab, prb) <- subs(b)
        if (aa == ab)
        merged <- pra.combineModules(prb).toList.map((aa, _))
      } yield merged

      if (merges.isEmpty) None
      else Some(merges.maxBy(_._1.asString.length)) // if more than 1 pick the one with the longest string
    }

    val allDepDoc = toMap.toList
      .map { case (g, map) =>
        val parts: List[(ArtifactOrProject, ProjectRecord)] = map.toList
          .sortBy(_._1.asString)
          .foldLeft(List.empty[(ArtifactOrProject, ProjectRecord)]) {
            case (Nil, ap) => List(ap)
            case (head :: tail, item) => merge(head, item) match {
              case None =>
                item :: head :: tail
              case Some(merged) =>
                merged :: tail
            }
          }
          .reverse

          val groupMap = yamlMap(parts.map { case (a, p) => (a.asString, p.toDoc) })

        (g.asString, groupMap)
      }
      .sorted

    yamlMap(allDepDoc, 2)
  }

  // Returns 1 if there is exactly one candidate that matches.
  def unversionedCoordinatesOf(g: MavenGroup, a: ArtifactOrProject): Option[UnversionedCoordinate] =
    toMap.get(g).flatMap { ap =>
      a.splitSubprojects match {
        case Nil =>
          ap.get(a).map(_.allDependencies(g, a)) match {
            case Some(h :: Nil) => Some(h)
            case other => println(other); None // 0 or more than one
          }
        case parts =>
          // This can be split, but may not be:
          val unsplit = ap.get(a).map(_.lang.unversioned(g, a)).toSet
          val uvcs = unsplit.union(parts.flatMap { case (proj, subproj) =>
            ap.get(proj)
              .map { pr => pr.getModules.filter(_ == subproj).map((_, pr.lang)) }
              .getOrElse(Nil)
              .map { case (m, lang) => lang.unversioned(g, proj, m) }
          }
          .toSet)
        if (uvcs.size == 1) Some(uvcs.head) else None
      }
    }

  def exportedUnversioned(u: UnversionedCoordinate,
    r: Replacements): Either[List[(MavenGroup, ArtifactOrProject)], List[UnversionedCoordinate]] =

    recordOf(u).flatMap(_.exports) match {
      case None => Right(Nil)
      case Some(l) =>
        def uv(g: MavenGroup, a: ArtifactOrProject): Option[UnversionedCoordinate] =
          unversionedCoordinatesOf(g, a).orElse(r.unversionedCoordinatesOf(g, a))

        val errs = l.filter { case (g, a) => uv(g, a).isEmpty }
        if (errs.nonEmpty) Left(errs)
        else Right(l.flatMap { case (g, a) => uv(g, a) })
    }

  private val coordToProj: Map[MavenCoordinate, ProjectRecord] =
    (for {
      (g, m) <- toMap.iterator
      (a, p) <- m.iterator
      mcoord <- p.versionedDependencies(g, a)
    } yield (mcoord -> p)).toMap

  private val unversionedToProj: Map[UnversionedCoordinate, ProjectRecord] =
    (for {
      (g, m) <- toMap.iterator
      (a, p) <- m.iterator
      uv <- p.allDependencies(g, a)
    } yield (uv -> p)).toMap

  val roots: Set[MavenCoordinate] = coordToProj.keySet
  val unversionedRoots: Set[UnversionedCoordinate] =
    unversionedToProj.iterator
      .collect { case (uv, pr) if pr.version.isEmpty => uv }
      .toSet
  /**
   * Note, if we implement this method with an unversioned coordinate,
   * we need to potentially remove the scala version to check the
   * ArtifactOrProject key
   */
  private def recordOf(m: UnversionedCoordinate): Option[ProjectRecord] =
    unversionedToProj.get(m)

  def languageOf(m: UnversionedCoordinate): Option[Language] =
    recordOf(m).map(_.lang)

  def excludes(m: UnversionedCoordinate): Set[UnversionedCoordinate] =
    recordOf(m).flatMap(_.exclude) match {
      case None => Set.empty
      case Some(uvs) =>
        uvs.map { case (g, a) =>
          unversionedCoordinatesOf(g, a)
            .getOrElse(UnversionedCoordinate(g, MavenArtifactId(a)))
        }.toSet
    }
}

object Dependencies {
  def apply(items: (MavenGroup, Map[ArtifactOrProject, ProjectRecord])*): Dependencies =
    Dependencies(items.groupBy(_._1)
      .map { case (g, pairs) =>
        val finalMap = pairs.map(_._2).reduce(_ ++ _)
        (g, finalMap)
      }
      .toMap)
}

case class BazelTarget(asString: String)

case class ReplacementRecord(
  lang: Language,
  target: BazelTarget)

case class Replacements(toMap: Map[MavenGroup, Map[ArtifactOrProject, ReplacementRecord]]) {
  val unversionedToReplacementRecord: Map[UnversionedCoordinate, ReplacementRecord] =
    toMap.flatMap { case (g, projs) =>
      projs.map { case (a, r) =>
        r.lang.unversioned(g, a) -> r
      }
    }

  def unversionedCoordinatesOf(g: MavenGroup, a: ArtifactOrProject): Option[UnversionedCoordinate] =
    for {
      m <- toMap.get(g)
      r <- m.get(a)
    } yield r.lang.unversioned(g, a)

  def get(uv: UnversionedCoordinate): Option[ReplacementRecord] =
    unversionedToReplacementRecord.get(uv)

  def toDoc: Doc = Doc.empty
}

object Replacements {
  val empty: Replacements = Replacements(Map.empty)
}

sealed abstract class VersionConflictPolicy(val asString: String) {
  /**
   * TODO we currenly only have policies that always keep roots,
   * if this invariant changes, Normalizer will need to change
   * the dead node elimination step
   */
  def resolve(root: Option[Version], s: Set[Version]): Either[String, Version]
}
object VersionConflictPolicy {
  def default: VersionConflictPolicy = Highest

  /**
   * there must be only 1 version.
   */
  case object Fail extends VersionConflictPolicy("fail") {
    def resolve(root: Option[Version], s: Set[Version]) = root match {
      case Some(v) if s.size == 1 && s(v) => Right(v)
      case None if s.size == 1 => Right(s.head)
      case _ => Left(s"multiple versions found in Fail policy, root: $root, transitive: ${s.toList.sorted}")
    }
  }
  /**
   * It a version is explicitly declared, it is always used,
   * otherwise there must be only 1 version.
   */
  case object Fixed extends VersionConflictPolicy("fixed") {
    def resolve(root: Option[Version], s: Set[Version]) = root match {
      case Some(v) => Right(v)
      case None if s.size == 1 => Right(s.head)
      case None => Left(s"fixed requires 1, or a declared version, found: ${s.toList.sorted}")
    }
  }
  /**
   * It a version is explicitly declared, it is always used,
   * otherwise we take the highest version.
   */
  case object Highest extends VersionConflictPolicy("highest") {
    def resolve(root: Option[Version], s: Set[Version]) = root match {
      case Some(v) => Right(v)
      case None => Right(s.max) // there must be at least one version, so this won't throw
    }
  }
}

case class DirectoryName(asString: String) {
  def parts: List[String] =
    asString.split('/').filter(_.nonEmpty).toList
}

object DirectoryName {
  def default: DirectoryName = DirectoryName("3rdparty/jvm")
}

sealed abstract class Transitivity(val asString: String)
object Transitivity {
  case object RuntimeDeps extends Transitivity("runtime_deps")
  case object Exports extends Transitivity("exports")
}

case class Options(
  versionConflictPolicy: Option[VersionConflictPolicy],
  thirdPartyDirectory: Option[DirectoryName],
  languages: Option[List[Language]],
  resolvers: Option[List[MavenServer]],
  transitivity: Option[Transitivity],
  buildHeader: Option[List[String]]) {

  def isDefault: Boolean =
    versionConflictPolicy.isEmpty &&
    thirdPartyDirectory.isEmpty &&
    languages.isEmpty &&
    resolvers.isEmpty &&
    transitivity.isEmpty &&
    buildHeader.isEmpty

  def getThirdPartyDirectory: DirectoryName =
    thirdPartyDirectory.getOrElse(DirectoryName.default)

  def getVersionConflictPolicy: VersionConflictPolicy =
    versionConflictPolicy.getOrElse(VersionConflictPolicy.default)

  def getLanguages: List[Language] = languages match {
    case None => List(Language.Java, Language.Scala(Version("2.11.8"), true))
    case Some(langs) => langs
  }
  def getResolvers: List[MavenServer] =
    resolvers.getOrElse(
      List(MavenServer("central", "default", "http://central.maven.org/maven2/")))

  def getTransitivity: Transitivity =
    transitivity.getOrElse(Transitivity.Exports)

  def getBuildHeader: String = buildHeader match {
    case Some(lines) => lines.mkString("\n")
    case None => ""
  }

  def toDoc: Doc = {
    val items = List(
      ("versionConflictPolicy",
        versionConflictPolicy.map { p => Doc.text(p.asString) }),
      ("thirdPartyDirectory",
        thirdPartyDirectory.map { tpd => quoteDoc(tpd.asString) }),
      ("resolvers",
        resolvers.map {
          case Nil => Doc.text("[]")
          case ms => (Doc.line +: vlist(ms.map(_.toDoc))).nest(2)
        }),
      ("languages",
        languages.map { ls => list(ls) { l => quoteDoc(l.asOptionsString) } }),
      ("buildHeader",
        buildHeader.map(list(_) { s => quoteDoc(s) })),
      ("transitivity", transitivity.map { t => Doc.text(t.asString) }))
        .sortBy(_._1)
        .collect { case (k, Some(v)) => (k, v) }

    // we can't pack resolvers (yet)
    packedYamlMap(items)
  }
}

object Options {
  def default: Options = Options(None, None, None, None, None, None)
}
