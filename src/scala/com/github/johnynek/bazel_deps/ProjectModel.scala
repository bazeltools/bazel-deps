package com.github.johnynek.bazel_deps

object ProjectModel extends MakeDeps {
  def servers: List[MavenServer] =
    List(MavenServer("central", "default", "http://central.maven.org/maven2/"))

  def model = {
    import MakeDeps.{java, scala, subprojects}

    val deps = Dependencies(
      subprojects(
        Language.Java,
        "org.eclipse.aether:aether-",
        List("api", "impl", "connector-basic", "transport-file", "transport-http"),
        "1.0.2.v20150114"),
      subprojects(
        Language.Scala(Version("2.11"), true),
        "io.circe:circe-",
        List("core", "generic", "parser"),
        "0.5.0-M2"),
      java("org.apache.maven:maven-aether-provider:3.1.0"),
      scala("org.scalacheck:scalacheck:1.12.0")
      )

    val replacements = Replacements(
      Map(
        MavenGroup("org.scala-lang") ->
          Map(
            ArtifactOrProject("scala-library") ->
              ReplacementRecord(Language.Scala(Version("2.11"), false), // this is not mangled
                BazelTarget("@scala//:lib/scala-library.jar")),
            ArtifactOrProject("scala-reflect") ->
              ReplacementRecord(Language.Scala(Version("2.11"), false), // this is not mangled
                BazelTarget("@scala//:lib/scala-reflect.jar"))),

      MavenGroup("org.scala-lang.modules") ->
        Map(ArtifactOrProject("scala-parser-combinators") ->
          ReplacementRecord(Language.Scala(Version("2.11"), true),
            BazelTarget("@scala//:lib/scala-parser-combinators_2.11-1.0.4.jar")))
              ))

    Model(deps, replacements = Some(replacements), None)
  }

  def getSettings(args: Array[String]) = {
    val workspacePath = args(0)
    val projectRoot = args(1)
    val thirdParty = args(2)
    (model, servers, workspacePath, projectRoot, thirdParty)
  }
}
