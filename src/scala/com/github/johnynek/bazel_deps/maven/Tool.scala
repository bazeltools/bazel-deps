package com.github.johnynek.bazel_deps
package maven

import scala.xml._

object Tool {

  def singleNode(nodes: NodeSeq): Node = {
    val res = nodes.lengthCompare(1)
    if (res == 0) nodes.head
    else if (res < 0) sys.error("missing node!")
    else sys.error("too many nodes!")
  }

  def singleText(nodes: NodeSeq): String =
    singleNode(nodes).text

  def optionalNode(nodes: NodeSeq): Option[Node] = {
    val res = nodes.lengthCompare(1)
    if (res == 0) Some(nodes.head)
    else if (res < 0) None
    else sys.error("too many nodes!")
  }

  def optionalText(nodes: NodeSeq): Option[String] =
    optionalNode(nodes).map(_.text)

  case class Project(
    path: String,
    name: Option[String],
    version: String,
    artifactId: String,
    groupId: String,
    packaging: String,
    props: Map[String, String],
    parentPath: Option[String], //fixme
    modules: Seq[String], //fixme
    dependencies: Seq[Dep] //fixme
  ) {
    def toDep: Dep = Dep(groupId, artifactId, version, None)

    private def dir: String = directoryFor(path).get

    lazy val parseModuleProjects: Seq[Project] =
      modules.map { m =>
        parse(s"$dir/$m/pom.xml")
      }
  }

  case class Dep(
    groupId: String,
    artifactId: String,
    version: String,
    scope: Option[String]) {

    def unScalaVersion(s: Language.Scala): Option[Dep] =
      s.removeSuffix(artifactId)
        .map(Dep(groupId, _, version, scope))

    def hasScalaBinaryVersion: Boolean =
      artifactId.endsWith("${scala.binary.version}")

    /**
     * Apply the properties to the strings in Dep
     */
    def resolve(props: Map[String, String]): Dep = {
      val Symbol = """\$\{(.+?)\}""".r
      def r(s: String): String = {
        Symbol.replaceAllIn(s, { m =>
          val k = m.group(1)
          props.getOrElse(k, "${" + k + "}")
        })
      }

      Dep(r(groupId), r(artifactId), r(version), scope)
    }
  }

  def parseDep(e: Node): Dep =
    Dep(singleText(e \ "groupId"),
      singleText(e \ "artifactId"),
      singleText(e \ "version"),
      optionalText(e \ "scope"))

  private def directoryFor(path: String): Option[String] = {
    val f = new java.io.File(path)
    if (f.isFile) Option(f.getParent) else None
  }

  def parse(path: String): Project = {
    val root = XML.loadFile(path)

    val name = optionalText(root \ "name")
    val version = singleText(root \ "version")
    val artifactId = singleText(root \ "artifactId")
    val groupId = singleText(root \ "groupId")
    val packaging = singleText(root \ "packaging")

    val parent = (root \ "parent" \ "relativePath").headOption.map(_.text)

    val localProps: Map[String, String] = (root \ "properties" \ "_")
      .map(node => (node.label, node.text)).toMap

    val baseDirectory = directoryFor(path).get

    val parentProps = parent.map(s => parse(baseDirectory + "/" + s)).map(_.props).getOrElse(Map.empty[String, String])

    val props = parentProps ++ localProps //fixme

    val modules = (root \ "modules" \ "module").map(_.text)
    val deps = (root \ "dependencies" \ "dependency").map(parseDep)

    Project(path, name, version, artifactId, groupId, packaging, props, parent, modules, deps)
  }

  def allDependencies(root: Project): Dependencies = {

    def allChildren(p: Project): Set[Project] =
      Set(p) ++ p.parseModuleProjects.flatMap(allChildren)

    val allProjs: Set[Project] = allChildren(root)
    val scalaVersion: Option[Language.Scala] =
      (allProjs.flatMap { _.props.get("scala.binary.version") }.toList) match {
        case Nil => None
        case v :: Nil => Some(Language.Scala(Version(v), mangle = true))
        case other => sys.error(s"Many scala versions: ${other.sorted}")
      }

    val localDeps: Map[Dep, Project] = allProjs.map { p => (p.toDep, p) }.toMap
    val localKeys = localDeps.keySet

    val externalDeps: Set[(Dep, Language)] =
      allProjs.flatMap { p =>
        p.dependencies.map { d =>
          val resDep = d.resolve(p.props)

          scalaVersion
            .flatMap { s => resDep.unScalaVersion(s).map((_, s)) }
            .getOrElse((resDep, Language.Java))
        }
      }
      .toSet[(Dep, Language)] // invariance of sets biting us
      .filterNot { case (d, _) => localKeys(d) }

    val parts: List[(MavenGroup, ArtifactOrProject, ProjectRecord)] =
      externalDeps.iterator.map { case (d, lang) =>
        (MavenGroup(d.groupId),
          ArtifactOrProject(d.artifactId),
          ProjectRecord(lang,
            Some(Version(d.version)),
            None,
            None,
            None))
      }
      .toList

    val asMap: Map[MavenGroup, Map[ArtifactOrProject, ProjectRecord]] =
      parts.groupBy(_._1)
        .mapValues { list =>
          list.map { case (_, a, p) =>
            (a, p)
          }.toMap
        }
    Dependencies(asMap)
  }


  def main(args: Array[String]): Unit = {
    val rootPom = parse(args(0))
    println(Model(allDependencies(rootPom), None, None).asString)
  }
}
