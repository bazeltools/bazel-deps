scala_library(
    name = "trymerge",
    srcs = ["TryMerge.scala"],
    visibility = ["//visibility:public"],
    exports = [
        "//3rdparty/jvm/org/typelevel:cats_core",
    ],
    deps = [
        "//3rdparty/jvm/org/typelevel:cats_core",
        "//3rdparty/jvm/org/typelevel:cats_kernel",
    ],
)

scala_library(
    name = "depsmodel",
    srcs = ["DepsModel.scala"],
    visibility = ["//visibility:public"],
    exports = [
        "//3rdparty/jvm/org/typelevel:cats_core",
        "//3rdparty/jvm/org/typelevel:paiges_core",
    ],
    deps = [
        ":trymerge",
        "//3rdparty/jvm/org/typelevel:cats_core",
        "//3rdparty/jvm/org/typelevel:cats_kernel",
        "//3rdparty/jvm/org/typelevel:paiges_core",
    ],
)

scala_library(
    name = "commands",
    srcs = ["Commands.scala"],
    exports = [
        ":depsmodel",
        "//3rdparty/jvm/com/monovore:decline",
    ],
    deps = [
        ":depsmodel",
        "//3rdparty/jvm/com/monovore:decline",
    ],
)

scala_library(
    name = "graph",
    srcs = ["Graph.scala"],
    visibility = ["//visibility:public"],
    exports = ["//3rdparty/jvm/org/typelevel:paiges_core"],
    deps = ["//3rdparty/jvm/org/typelevel:paiges_core"],
)

scala_repl(
    name = "repl",
    deps = [":graph"],
)

scala_library(
    name = "resolver",
    srcs = ["Resolver.scala"],
    visibility = ["//visibility:public"],
    deps = [
        ":depsmodel",
        ":graph",
        "//3rdparty/jvm/org/typelevel:cats_core",
    ],
)

scala_library(
    name = "settings_loader",
    srcs = ["SettingsLoader.scala"],
    visibility = ["//visibility:public"],
    exports = ["//3rdparty/jvm/org/apache/maven:maven_settings"],
    deps = [
        "//3rdparty/jvm/org/apache/maven:maven_settings",
        "//3rdparty/jvm/org/apache/maven:maven_settings_builder",
        "//3rdparty/jvm/org/slf4j:slf4j_api",
    ],
)

scala_library(
    name = "coursier_resolver",
    srcs = ["CoursierResolver.scala"],
    visibility = ["//visibility:public"],
    deps = [
        ":depsmodel",
        ":graph",
        ":resolver",
        ":settings_loader",
        "//3rdparty/jvm/io/get_coursier:coursier",
        "//3rdparty/jvm/io/get_coursier:coursier_cache",
        "//3rdparty/jvm/io/get_coursier:coursier_core",
        "//3rdparty/jvm/io/get_coursier:coursier_util",
        "//3rdparty/jvm/org/slf4j:slf4j_api",
        "//3rdparty/jvm/org/typelevel:cats_core",
    ],
)

scala_library(
    name = "gradle_resolver",
    srcs = [
        "GradleLockDependency.scala",
        "GradleLockFile.scala",
        "GradleResolver.scala",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":circeyaml",
        ":depsmodel",
        ":graph",
        ":resolver",
        ":trymerge",
        "//3rdparty/jvm/io/circe:circe_core",
        "//3rdparty/jvm/io/circe:circe_generic",
        "//3rdparty/jvm/io/circe:circe_jawn",
        "//3rdparty/jvm/org/slf4j:slf4j_api",
        "//3rdparty/jvm/org/typelevel:cats_core",
        "//3rdparty/jvm/org/typelevel:cats_free",
    ],
)

scala_library(
    name = "aether_resolver",
    srcs = ["AetherResolver.scala"],
    visibility = ["//visibility:public"],
    exports = [
        "//3rdparty/jvm/org/apache/maven:maven_settings",
        "//3rdparty/jvm/org/eclipse/aether:aether_spi",
    ],
    deps = [
        ":depsmodel",
        ":graph",
        ":resolver",
        ":settings_loader",
        "//3rdparty/jvm/org/apache/maven:maven_aether_provider",
        "//3rdparty/jvm/org/apache/maven:maven_settings",
        "//3rdparty/jvm/org/apache/maven:maven_settings_builder",
        "//3rdparty/jvm/org/eclipse/aether:aether_api",
        "//3rdparty/jvm/org/eclipse/aether:aether_connector_basic",
        "//3rdparty/jvm/org/eclipse/aether:aether_impl",
        "//3rdparty/jvm/org/eclipse/aether:aether_spi",
        "//3rdparty/jvm/org/eclipse/aether:aether_transport_file",
        "//3rdparty/jvm/org/eclipse/aether:aether_transport_http",
        "//3rdparty/jvm/org/eclipse/aether:aether_util",
        "//3rdparty/jvm/org/eclipse/sisu:org_eclipse_sisu_plexus",
        "//3rdparty/jvm/org/slf4j:slf4j_api",
        "//3rdparty/jvm/org/sonatype/plexus:plexus_cipher",
        "//3rdparty/jvm/org/sonatype/plexus:plexus_sec_dispatcher",
        "//3rdparty/jvm/org/typelevel:cats_core",
    ],
)

scala_library(
    name = "io",
    srcs = ["IO.scala"],
    visibility = ["//visibility:public"],
    exports = [
        "//3rdparty/jvm/org/typelevel:cats_free",
    ],
    deps = [
        "//3rdparty/jvm/org/slf4j:slf4j_api",
        "//3rdparty/jvm/org/typelevel:cats_core",
        "//3rdparty/jvm/org/typelevel:cats_free",
    ],
)

scala_library(
    name = "normalizer",
    srcs = ["Normalizer.scala"],
    visibility = ["//visibility:public"],
    deps = [
        ":depsmodel",
        ":graph",
        "//3rdparty/jvm/org/typelevel:paiges_core",
    ],
)

scala_library(
    name = "writer",
    srcs = [
        "Label.scala",
        "Target.scala",
        "Writer.scala",
    ],
    scalacopts = ["-Ypartial-unification"],
    visibility = ["//visibility:public"],
    deps = [
        ":depsmodel",
        ":graph",
        ":io",
        "//3rdparty/jvm/io/circe:circe_core",
        "//3rdparty/jvm/io/circe:circe_generic",
        "//3rdparty/jvm/org/slf4j:slf4j_api",
        "//3rdparty/jvm/org/typelevel:cats_core",
        "//3rdparty/jvm/org/typelevel:cats_free",
        "//3rdparty/jvm/org/typelevel:paiges_core",
    ],
)

scala_library(
    name = "makedeps",
    srcs = [
        "FormatDeps.scala",
        "MakeDeps.scala",
        "MergeDeps.scala",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":aether_resolver",
        ":circeyaml",
        ":commands",
        ":coursier_resolver",
        ":createpom",
        ":decoders",
        ":depsmodel",
        ":gradle_resolver",
        ":graph",
        ":io",
        ":normalizer",
        ":resolver",
        ":writer",
        "//3rdparty/jvm/io/circe:circe_core",
        "//3rdparty/jvm/io/circe:circe_generic",
        "//3rdparty/jvm/io/circe:circe_jawn",
        "//3rdparty/jvm/io/get_coursier:coursier_cache",
        "//3rdparty/jvm/io/get_coursier:coursier_core",
        "//3rdparty/jvm/io/get_coursier:coursier_util",
        "//3rdparty/jvm/org/eclipse/aether:aether_api",
        "//3rdparty/jvm/org/slf4j:slf4j_api",
        "//3rdparty/jvm/org/typelevel:cats_core",
        "//3rdparty/jvm/org/typelevel:cats_free",
    ],
)

scala_binary(
    name = "parseproject",
    srcs = ["ParseProject.scala"],
    main_class = "com.github.johnynek.bazel_deps.ParseProject",
    resource_strip_prefix = package_name(),
    resources = [
        "templates/external_workspace_backend.bzl",
        "templates/jar_artifact_backend.bzl",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":commands",
        ":makedeps",
        "//3rdparty/jvm/org/slf4j:slf4j_simple",
    ],
)

scala_library(
    name = "circeyaml",
    srcs = ["CirceYaml.scala"],
    visibility = ["//visibility:public"],
    deps = [
        "//3rdparty/jvm/com/fasterxml/jackson/core:jackson_core",
        "//3rdparty/jvm/com/fasterxml/jackson/core:jackson_databind",
        "//3rdparty/jvm/com/fasterxml/jackson/dataformat:jackson_dataformat_yaml",
        "//3rdparty/jvm/io/circe:circe_core",
        "//3rdparty/jvm/io/circe:circe_jackson25",
        "//3rdparty/jvm/org/typelevel:cats_core",
    ],
)

scala_library(
    name = "decoders",
    srcs = ["Decoders.scala"],
    visibility = ["//visibility:public"],
    deps = [
        ":depsmodel",
        ":gradle_resolver",
        ":trymerge",
        "//3rdparty/jvm/io/circe:circe_core",
        "//3rdparty/jvm/io/circe:circe_generic",
    ],
)

scala_library(
    name = "createpom",
    srcs = ["CreatePom.scala"],
    visibility = ["//visibility:public"],
    deps = [
        ":depsmodel",
        ":graph",
        "//3rdparty/jvm/org/scala_lang/modules:scala_xml",
    ],
)
