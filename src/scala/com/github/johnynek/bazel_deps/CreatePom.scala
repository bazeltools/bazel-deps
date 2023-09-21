package com.github.johnynek.bazel_deps
import scala.xml._

object CreatePom {
  private def tagText(nm: String, txt: String): Elem =
      Elem(prefix = null, label = nm, attributes = Null, scope = TopScope, Text(txt))

  private def labelWithChildren(name: String, children: List[Elem]): Elem =
      Elem(prefix = null, label = name, attributes = Null, scope = TopScope, children: _*)

  implicit class MavenCoordinateExtension(private val self: MavenCoordinate)
      extends AnyVal {
    def toXml: Elem = {
      val children = ({
          // maybe add packaging type
          val pack = self.artifact.packaging
          if (pack != "jar") {
            // don't add jar, which is the default
            tagText("type", pack) :: Nil
          }
          else Nil
        }) :::
        // maybe add classifier
        (self.artifact.classifier match {
          case None => Nil
          case Some(c) => 
            tagText("classifier", c) :: Nil
        }) :::
        // put version last
        tagText("version", self.version.asString) ::
        Nil

      labelWithChildren("dependency",
        tagText("groupId", self.group.asString) ::
        tagText("artifactId", self.artifact.artifactId) ::
        children
      )
    }
  }

  def translate(dependencies: Graph[MavenCoordinate, Unit]): String = {
    val mavenCoordinateXml = dependencies.nodes.toList.sorted.map { d =>
      d.toXml
    }

    val pomXml = labelWithChildren("project",
      tagText("modelVersion", "4.0.0") ::
      labelWithChildren("dependencies", mavenCoordinateXml) ::
      Nil)

    val p = new scala.xml.PrettyPrinter(80, 2)
    p.format(pomXml)
  }

  def writeIO(dependencies: Graph[MavenCoordinate, Unit], path: FS.Path): FS.Result[Unit] =
    FS.writeUtf8(path, translate(dependencies))

  def apply(dependencies: Graph[MavenCoordinate, Unit], path: String): Unit = {
    scala.xml.XML.save(
      path,
      scala.xml.XML.loadString(translate(dependencies)),
      "UTF-8",
      true
    )
  }
}
