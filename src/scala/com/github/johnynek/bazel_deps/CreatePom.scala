package com.github.johnynek.bazel_deps
import scala.xml._

object CreatePom {
  implicit class MavenCoordinateExtension(private val self: MavenCoordinate)
      extends AnyVal {
    def toXml: Elem = {
      if (self.artifact.classifier==None) {
        <dependency>
          <groupId>{self.group.asString}</groupId>
          <artifactId>{self.artifact.artifactId}</artifactId>
          <type>{self.artifact.packaging}</type>
          <version>{self.version.asString}</version>
        </dependency>
      } else {
        <dependency>
          <groupId>{self.group.asString}</groupId>
          <artifactId>{self.artifact.artifactId}</artifactId>
          <type>{self.artifact.packaging}</type>
          <classifier>{self.artifact.classifier.getOrElse(None)}</classifier>
          <version>{self.version.asString}</version>
        </dependency>
    }
  }
  }

  def translate(dependencies: Graph[MavenCoordinate, Unit]): String = {
    val mavenCoordinateXml = dependencies.nodes.toList.sorted.map { d =>
      d.toXml
    }

    val pomXml = <project>
      <modelVersion>4.0.0</modelVersion>

      <dependencies>
        {mavenCoordinateXml}
      </dependencies>
    </project>

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
