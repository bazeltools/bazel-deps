package com.github.johnynek.bazel_deps

object ProjectModel extends MakeDeps {
  def servers: List[MavenServer] =
    List(MavenServer("central", "default", "http://central.maven.org/maven2/"))

  def model = {
    import MakeDeps.{java, scala}

    val deps = Dependencies(
      MavenGroup("org.eclipse.aether") ->
        Map(ArtifactOrProject("aether") ->
          ProjectRecord(
            Language.Java,
            Version("1.0.2.v20150114"),
            List("api", "impl", "connector-basic", "transport-file", "transport-http").map(Subproject(_)))),

      java("org.apache.maven:maven-aether-provider:3.1.0"),
      scala("org.scalacheck:scalacheck:1.12.0")
      )

    val replacements = Replacements(
      Map(
        MavenGroup("org.scala-lang") ->
          Map(ArtifactOrProject("scala-library") ->
            // Actually, this is not versioned like a scala library, so we claim java here
            ReplacementRecord(Language.Java,
              BazelTarget("@scala//:lib/scala-library.jar"))),

      MavenGroup("org.scala-lang.modules") ->
        Map(ArtifactOrProject("scala-parser-combinators") ->
          ReplacementRecord(Language.Scala(Version("2.11")),
            BazelTarget("@scala//:lib/scala-parser-combinators_2.11-1.0.4.jar")))))

    Model(deps, replacements = Some(replacements), None)
  }

  def getSettings(args: Array[String]) = {
    val workspacePath = args(0)
    val projectRoot = args(1)
    val thirdParty = args(2)
    (model, servers, workspacePath, projectRoot, thirdParty)
  }
}
