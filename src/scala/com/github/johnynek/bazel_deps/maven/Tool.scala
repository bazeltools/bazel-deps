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
  )

  case class Dep(
    artifactId: String,
    groupId: String,
    version: String)

  def parseDep(e: Node, props: Map[String, String]): Dep = {
    val Symbol = """\$\{(.+?)\}""".r
    def resolve(name: String): String = {
      val s = singleText(e \ name)
      Symbol.replaceAllIn(s, { m =>
        val k = m.group(1)
        props.getOrElse(k, "${" + k + "}")
      })
    }

    Dep(resolve("artifactId"), resolve("groupId"), resolve("version"))
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

    def directoryFor(path: String): Option[String] = {
      val f = new java.io.File(path)
      if (f.isFile) Option(f.getParent) else None
    }

    val baseDirectory = directoryFor(path).get

    val parentProps = parent.map(s => parse(baseDirectory + "/" + s)).map(_.props).getOrElse(Map.empty[String, String])

    val props = parentProps ++ localProps //fixme

    val modules = (root \ "modules" \ "module").map(_.text)
    val deps = (root \ "dependencies" \ "dependency").map(parseDep(_, props))

    Project(path, name, version, artifactId, groupId, packaging, props, parent, modules, deps)
  }

  def main(args: Array[String]): Unit = {
    //println(parse("/Users/erik/stripe/decibel/pom.xml"))
    println(parse("/Users/erik/stripe/decibel/decibel-summingbird/pom.xml"))
    //println(parse("/Users/erik/stripe/decibel/parents/pom.xml"))
  }
}
