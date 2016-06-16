package com.github.johnynek.bazel_deps

case class Model(
  dependencies: Dependencies,
  replacements: Option[Replacements],
  options: Option[Options])

case class MavenGroup(asString: String)
case class ArtifactOrProject(asString: String)
case class Subproject(asString: String)
case class Version(asString: String)
case class MavenArtifactId(project: ArtifactOrProject, subproject: Option[Subproject]) {
  def asString: String = subproject match {
    case None => project.asString
    case Some(sb) => s"${project.asString}-{sb.asString}"
  }
}

case class MavenCoordinate(group: MavenGroup, artifact: MavenArtifactId, version: Version) {
  def asString: String = s"${group.asString}:${artifact.asString}:${version.asString}"
  /**
   * This is a bazel-safe name to use as a remote repo name
   */
  def toBazelRepoName: String = ???
}

object MavenCoordinate {
  def apply(s: String): MavenCoordinate = {
    s.split(":") match {
      case Array(g, a, v) => MavenCoordinate(MavenGroup(g), MavenArtifactId(ArtifactOrProject(a), None), Version(v))
      case other => sys.error(s"expected exactly three :, got $s")
    }
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

sealed abstract class VersionConflictPolicy
object VersionConflictPolicy {
  case object Fixed extends VersionConflictPolicy
  case object Highest extends VersionConflictPolicy
  case object Semantic extends VersionConflictPolicy
}

case class DirectoryName(asString: String)
case class TargetPattern(asString: String)
case class LanguageOption(version: Option[Version], targetPattern: Option[TargetPattern])

case class Options(
  versionConflictPolicy: Option[VersionConflictPolicy],
  thirdPartyDirectory: Option[DirectoryName],
  languages: Option[Map[Language, LanguageOption]])
