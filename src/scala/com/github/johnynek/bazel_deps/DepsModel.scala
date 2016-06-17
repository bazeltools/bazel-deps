package com.github.johnynek.bazel_deps

import scala.util.Try

case class Model(
  dependencies: Dependencies,
  replacements: Option[Replacements],
  options: Option[Options]) {

  def getOptions: Options =
    options.getOrElse(Options.default)
}

case class MavenGroup(asString: String)
case class ArtifactOrProject(asString: String)
case class Subproject(asString: String)
case class Version(asString: String)

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

case class MavenArtifactId(project: ArtifactOrProject, subproject: Option[Subproject]) {
  def asString: String = subproject match {
    case None => project.asString
    case Some(sb) => s"${project.asString}-{sb.asString}"
  }
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
      case Array(g, a, v) => MavenCoordinate(MavenGroup(g), MavenArtifactId(ArtifactOrProject(a), None), Version(v))
      case other => sys.error(s"expected exactly three :, got $s")
    }
  }
  def apply(u: UnversionedCoordinate, v: Version): MavenCoordinate =
    MavenCoordinate(u.group, u.artifact, v)

  implicit def mvnCoordOrd: Ordering[MavenCoordinate] = Ordering.by { m: MavenCoordinate =>
    (m.group.asString, m.artifact.asString, m.version)
  }
}

sealed abstract class Language
object Language {
  case object Java extends Language
  case object Scala extends Language
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
  modules: List[Subproject],
  excludes: List[UnversionedCoordinate])

case class Dependencies(toMap: Map[MavenGroup, Map[ArtifactOrProject, ProjectRecord]]) {
  def coordinates(o: Options): Map[MavenCoordinate, (MavenGroup, ArtifactOrProject)] = ???
}

case class BazelTarget(asString: String)

case class ReplacementRecord(
  lang: Language,
  target: BazelTarget)

case class Replacements(toMap: Map[MavenGroup, Map[ArtifactOrProject, ReplacementRecord]])

sealed abstract class VersionConflictPolicy {
  def resolve(root: Option[Version], s: Set[Version]): Either[String, Version]
}
object VersionConflictPolicy {
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
case class LanguageOption(version: Option[Version], targetPattern: Option[TargetPattern])

case class Options(
  versionConflictPolicy: Option[VersionConflictPolicy],
  thirdPartyDirectory: Option[DirectoryName],
  languages: Option[Map[Language, LanguageOption]]) {

  def getVersionConflictPolicy: VersionConflictPolicy =
    versionConflictPolicy.getOrElse(VersionConflictPolicy.Fail)
}

object Options {
  def default: Options = Options(None, None, None)
}
