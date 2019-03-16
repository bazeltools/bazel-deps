package com.github.johnynek.bazel_deps
package maven

import cats.implicits._
import io.circe.jawn.JawnParser
import java.io._
import scala.sys.process.Process
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
    dependencies: List[Dep] //fixme
  ) {
    def toDep: Dep = Dep(groupId, artifactId, version, None, Some(packaging), None)

    def dir: String = directoryFor(path).getOrElse(".")

    lazy val parseModuleProjects: List[Project] =
      modules.map { m =>
        parse(s"$dir/$m/pom.xml")
      }.toList

    def allProps: Stream[Map[String, String]] =
      props #:: (parseModuleProjects.toStream.flatMap(_.allProps))

    def findProp(p: String): Option[String] =
      allProps.flatMap(_.get(p)).headOption
  }

  case class Dep(
    groupId: String,
    artifactId: String,
    version: String,
    scope: Option[String],
    packaging: Option[String], // corresponds to "<type>"
    classifier: Option[String]) {

    def unScalaVersion(s: Language.Scala): Option[Dep] =
      s.removeSuffix(artifactId)
        .map(Dep(groupId, _, version, scope, packaging, classifier))

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

      Dep(r(groupId), r(artifactId), r(version), scope,packaging.map(r), classifier.map(r))
    }
  }

  def parseDep(e: Node): Dep =
    Dep(singleText(e \ "groupId"),
      singleText(e \ "artifactId"),
      singleText(e \ "version"),
      optionalText(e \ "scope"),
      optionalText(e \ "type"), // aka "packaging"
      optionalText(e \ "classifier"))

  private def directoryFor(path: String): Option[String] = {
    val f = new File(path)
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
    val deps = (root \ "dependencies" \ "dependency").map(parseDep).toList

    Project(path, name, version, artifactId, groupId, packaging, props, parent, modules, deps)
  }

  def allProjects(root: Project): Set[Project] = {
    def allChildren(p: Project): Set[Project] =
      Set(p) ++ p.parseModuleProjects.flatMap(allChildren)
    allChildren(root)
  }

  def allDependencies(root: Project): Dependencies = {

    val allProjs = allProjects(root)
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
          ArtifactOrProject(MavenArtifactId(
            d.artifactId, d.packaging.getOrElse(MavenArtifactId.defaultPackaging), d.classifier)),
          ProjectRecord(lang,
            Some(Version(d.version)),
            None,
            None,
            None,
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

  def writeDependencies(opt: Option[Options], proj: Project): IO.Result[Unit] = {
    val yamlPath = IO.path(s"${proj.dir}/dependencies.yaml")
    def contents: String =
      Model(allDependencies(proj),
        None,
        opt
      ).toDoc.render(80)

    IO.writeUtf8(yamlPath, contents)
  }

  def bazelize(s: String): String =
    s.map { c =>
      if (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9')) c else '_'
    }

  def writeWorkspace(proj: Project): IO.Result[Unit] = {
    val workspacePath = IO.path(s"${proj.dir}/WORKSPACE")
    val bazelName = bazelize(proj.name.getOrElse("default_name"))

    def contents: String = s"""
workspace(name = "$bazelName")

git_repository(
  name = "io_bazel_rules_scala",
  remote = "https://github.com/bazelbuild/rules_scala",
  commit = "73743b830ae98d13a946b25ad60cad5fee58e6d3",
)

load("@io_bazel_rules_scala//scala:scala.bzl", "scala_repositories")
scala_repositories()

# use bazel-deps to manage transitive maven dependencies
# https://github.com/johnynek/bazel-deps
load("//3rdparty:workspace.bzl", "maven_dependencies")
load("//3rdparty:maven_load.bzl", "maven_load")
maven_dependencies(maven_load)
"""

    IO.writeUtf8(workspacePath, contents)
  }

  val DefaultHeader: String =
    """load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library", "scala_binary", "scala_test")"""

  def writeBuilds(root: Project, header: Option[String] = Some(DefaultHeader)): IO.Result[Unit] = {

    /**
     * load all project:
     *   - get their maven coords
     *   - get their files (*.scala, *.java)
     *   - get their explicit deps
     *   - &c
     *
     * for each project:
     *   - create the transitive closure of projects
     *   - partition closure into internal/external
     *   - for external, translate to maven coord -> build label
     *   - for internal, find corresponding project -> build label
     *   - (testing????)
     *   - write build file
     */
    val rootPath = IO.path(root.dir)
    val allProjs = allProjects(root)
    val localDeps: Map[Dep, Project] = allProjs.map { p => (p.toDep, p) }.toMap
    val localKeys = localDeps.keySet

    def labelFor(p: Project, targetName: String): IO.Result[Label] = {
      val pdir = p.dir
      if (pdir.startsWith(root.dir)) IO.const(Label(None, IO.path(pdir.drop(root.dir.length)), targetName))
      else IO.failed(new Exception(s"$pdir is not inside root: ${root.dir}"))
    }

    val scalaBinaryVersion = root.findProp("scala.binary.version").get
    val scalaLang = Language.Scala(Version(scalaBinaryVersion), true)

    val externalDeps: Map[Dep, Label] =
      allProjs.iterator.flatMap { p =>
        p.dependencies.map { d => (d, d.resolve(p.props)) }
      }
      .filterNot { case (d, resd) => localKeys(d) }
      .map { case (dep, resolvedDep) =>
        val lang =
          if (dep.hasScalaBinaryVersion) scalaLang
          else Language.Java

        (dep, Label.localTarget(List("3rdparty", "jvm"),
          UnversionedCoordinate(MavenGroup(resolvedDep.groupId), MavenArtifactId(resolvedDep.artifactId)),
          lang))
      }
      .toMap

    def getLabelFor(d: Dep): IO.Result[Label] =
      for {
        loc <- localDeps.get(d).traverse(labelFor(_, "main"))
        ex = externalDeps.get(d)
        res <- (ex.orElse(loc) match {
          case None => IO.failed(new Exception(s"Could not find local or remote dependency $d"))
          case Some(t) => IO.const(t)
        })
      } yield res

    def writeBuild(proj: Project): IO.Result[Unit] = {
      val buildPath = IO.path(s"${proj.dir}/BUILD")
      /**
       * 1) in proj.dir/BUILD make main, test targets
       */
      val depLabels = proj.dependencies.traverse(getLabelFor)

      def mainTarget(labs: List[Label]): IO.Result[Target] =
        labelFor(proj, "main").map { lab =>
          Target(scalaLang,
            lab,
            visibility = Target.Visibility.Public,
            deps = labs.toSet,
            sources = Target.SourceList.Globs(List("src/main/**/*.scala", "src/main/**/*.java")))
        }

      val thisBuild = if (proj.packaging == "jar") {
        def contents(t: Target): String =
          header.fold("\n")(_ + "\n") ++ t.toDoc.render(60)

        for {
          labs <- depLabels
          targ <- mainTarget(labs)
          _ <- IO.writeUtf8(buildPath, contents(targ))
        } yield ()
      } else IO.unit

      val childBuilds = proj.parseModuleProjects.traverse(writeBuild)

      for {
        _ <- thisBuild
        _ <- childBuilds
      } yield ()
    }

    writeBuild(root) //fixme
  }

  def cleanUpBuild(proj: Project): IO.Result[Unit] =
    for {
      _ <- IO.recursiveRmF(IO.path(s"${proj.dir}/BUILD"))
      _ <- IO.recursiveRmF(IO.path(s"${proj.dir}/WORKSPACE"))
      _ <- IO.recursiveRmF(IO.path(s"${proj.dir}/dependencies.yaml"))
    } yield ()

  def main(args: Array[String]): Unit =
    if (args.length == 0) {
      println("no pom.xml path provided!")
      System.exit(1)
    } else {
      val pomPath = args(0)
      val rootProject = parse(pomPath)

      val options = if (args.length >= 2) {
        val optFile = args(1)
        val parser = if (optFile.endsWith(".json")) new JawnParser else Yaml
        parser.decode(Model.readFile(new File(optFile)).get)(Decoders.optionsDecoder) match {
          case Left(err) => sys.error("could not decode $optFile. $err")
          case Right(opt) =>
            if (opt.isDefault) None
            else Some(opt)
        }
      } else None

      val io = for {
        _ <- cleanUpBuild(rootProject)
        _ <- writeDependencies(options, rootProject)
        _ <- writeWorkspace(rootProject)
        _ <- writeBuilds(rootProject)
      } yield ()

      IO.run(io, new File("/"))(_ => println("done"))
    }
}
