package com.github.johnynek.bazel_deps

import scala.util.Try

case class Model(
  dependencies: Dependencies,
  replacements: Option[Replacements],
  options: Option[Options]) {

  def getOptions: Options =
    options.getOrElse(Options.default)

  def languageOf(m: MavenCoordinate): Language =
    dependencies.recordOf(m) match {
      case Some(pr) => pr.lang
      case None => Language.default
    }
  def getReplacements: Replacements =
    replacements.getOrElse(Replacements.empty)
}

case class MavenGroup(asString: String)
case class ArtifactOrProject(asString: String)
case class Subproject(asString: String)
case class Version(asString: String)
case class Sha1Value(toHex: String)
case class MavenServer(id: String, contentType: String, url: String)

object Version {
  implicit def versionOrdering: Ordering[Version] = {
    val strNumOrd: Ordering[String] = new Ordering[String] {
      def compare(left: String, right: String): Int = {
        def toLong(s: String): Either[String, Long] =
          Try(s.toLong).toOption.fold(Left(s): Either[String, Long])(Right(_))

        (toLong(left), toLong(right)) match {
          case (Right(a), Right(b)) => java.lang.Long.compare(a, b)
          case (Right(_), Left(_)) => 1 // put non number before number
          case (Left(_), Right(_)) => -1
          case (Left(a), Left(b)) => a.compareTo(b)
        }
      }
    }
    Ordering.by { v: Version => v.asString.split('.').toIterable }(Ordering.Iterable(strNumOrd))
  }
}

case class MavenArtifactId(asString: String) {
  def addSuffix(s: String): MavenArtifactId = MavenArtifactId(asString + s)
}

object MavenArtifactId {
  def apply(a: ArtifactOrProject): MavenArtifactId = MavenArtifactId(a.asString)
  def apply(a: ArtifactOrProject, s: Subproject): MavenArtifactId = MavenArtifactId(a.asString + "-" + s.asString)
}

case class MavenCoordinate(group: MavenGroup, artifact: MavenArtifactId, version: Version) {
  def unversioned: UnversionedCoordinate = UnversionedCoordinate(group, artifact)
  def asString: String = s"${group.asString}:${artifact.asString}:${version.asString}"
  /**
   * This is a bazel-safe name to use as a remote repo name
   */
  def toBazelRepoName: String =
    unversioned.asString.map {
      case '.' => "_"  // todo, we should have something such that if a != b this can't be equal, but this can
      case '-' => "_"
      case ':' => "_"
      case other => other
    }
    .mkString
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
  def mavenCoord(g: MavenGroup, a: ArtifactOrProject, v: Version): MavenCoordinate
  def mavenCoord(g: MavenGroup, a: ArtifactOrProject, sp: Subproject, v: Version): MavenCoordinate
  def unversioned(g: MavenGroup, a: ArtifactOrProject): UnversionedCoordinate
}

object Language {
  def default: Language = Java

  case object Java extends Language {
    def mavenCoord(g: MavenGroup, a: ArtifactOrProject, v: Version): MavenCoordinate =
      MavenCoordinate(g, MavenArtifactId(a), v)

    def mavenCoord(g: MavenGroup, a: ArtifactOrProject, sp: Subproject, v: Version): MavenCoordinate =
      MavenCoordinate(g, MavenArtifactId(a, sp), v)

    def unversioned(g: MavenGroup, a: ArtifactOrProject): UnversionedCoordinate =
      UnversionedCoordinate(g, MavenArtifactId(a))
  }

  case class Scala(v: Version) extends Language {
    val major = v.asString.split('.') match {
      case Array("2", x) if (x.toInt >= 10) => s"2.$x"
      case Array("2", x, _) if (x.toInt >= 10) => s"2.$x"
      case _ => sys.error(s"unsupported scala version: ${v.asString}")
    }
    private val suffix = s"_$major"

    def unversioned(g: MavenGroup, a: ArtifactOrProject): UnversionedCoordinate =
      UnversionedCoordinate(g, MavenArtifactId(a).addSuffix(suffix))

    def mavenCoord(g: MavenGroup, a: ArtifactOrProject, v: Version): MavenCoordinate =
      MavenCoordinate(g, MavenArtifactId(a).addSuffix(suffix), v)

    def mavenCoord(g: MavenGroup, a: ArtifactOrProject, sp: Subproject, v: Version): MavenCoordinate =
      MavenCoordinate(g, MavenArtifactId(a, sp).addSuffix(suffix), v)

    def removeSuffix(s: String): Option[String] =
      if (s.endsWith(suffix)) Some(s.dropRight(suffix.size))
      else None
  }
}

sealed abstract class Explicitness

object Explicitness {
  case object Explicit extends Explicitness
  case object Implicit extends Explicitness
}

case class UnversionedCoordinate(group: MavenGroup, artifact: MavenArtifactId) {
  def asString: String = s"${group.asString}:${artifact.asString}"
}

case class ProjectRecord(
  lang: Language,
  version: Version,
  modules: List[Subproject])

case class Dependencies(toMap: Map[MavenGroup, Map[ArtifactOrProject, ProjectRecord]]) {
  private val coordToProj: Map[MavenCoordinate, ProjectRecord] =
    (for {
      (g, m) <- toMap.iterator
      (a, p) <- m.iterator
      mcoord <- mavens(g, a, p)
    } yield (mcoord -> p)).toMap

  private def mavens(g: MavenGroup, ap: ArtifactOrProject, pr: ProjectRecord): List[MavenCoordinate] =
    pr.modules match {
      case Nil => List(pr.lang.mavenCoord(g, ap, pr.version))
      case mods => mods.map { m => pr.lang.mavenCoord(g, ap, m, pr.version) }
    }

  def roots: Set[MavenCoordinate] = coordToProj.keySet
  def recordOf(m: MavenCoordinate): Option[ProjectRecord] = coordToProj.get(m)
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
  private val uvToRec: Map[UnversionedCoordinate, ReplacementRecord] =
    toMap.flatMap { case (g, projs) =>
      projs.map { case (a, r) =>
        r.lang.unversioned(g, a) -> r
      }
    }

  def get(uv: UnversionedCoordinate): Option[ReplacementRecord] =
    uvToRec.get(uv)
}

object Replacements {
  val empty: Replacements = Replacements(Map.empty)
}

sealed abstract class VersionConflictPolicy {
  def resolve(root: Option[Version], s: Set[Version]): Either[String, Version]
}
object VersionConflictPolicy {
  def default: VersionConflictPolicy = Highest

  case object Fail extends VersionConflictPolicy {
    def resolve(root: Option[Version], s: Set[Version]) = root match {
      case Some(v) if s.size == 1 && s(v) => Right(v)
      case None if s.size == 1 => Right(s.head)
      case _ => Left(s"multiple versions found in Fail policy, root: $root, transitive: ${s.toList.sorted}")
    }
  }
  case object Fixed extends VersionConflictPolicy {
    def resolve(root: Option[Version], s: Set[Version]) = root match {
      case Some(v) => Right(v)
      case None if s.size == 1 => Right(s.head)
      case None => Left(s"fixed requires 1, or a declared version, found: ${s.toList.sorted}")
    }
  }
  case object Highest extends VersionConflictPolicy {
    def resolve(root: Option[Version], s: Set[Version]) = root match {
      case Some(v) => Right(v)
      case None => Right(s.max)
    }
  }
}

case class DirectoryName(asString: String)
case class TargetPattern(asString: String)
case class LanguageOption(targetPattern: Option[TargetPattern])

case class Options(
  versionConflictPolicy: Option[VersionConflictPolicy],
  thirdPartyDirectory: Option[DirectoryName],
  languages: Option[List[(Language, LanguageOption)]]) {

  def getVersionConflictPolicy: VersionConflictPolicy =
    versionConflictPolicy.getOrElse(VersionConflictPolicy.default)

  def getLanguages: List[(Language, LanguageOption)] = languages match {
    case None => List(Language.Java -> LanguageOption(None))
    case Some(langs) => langs
  }
}

object Options {
  def default: Options = Options(None, None, None)
}
