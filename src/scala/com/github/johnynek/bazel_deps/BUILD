scala_library(name = "depsmodel",
              srcs = ["DepsModel.scala"],
              deps = [
                  "//3rdparty/jvm/org/typelevel:cats_core",
                  "//3rdparty/jvm/org/typelevel:cats_kernel",
                  "//3rdparty/jvm/org/typelevel:paiges_core",
              ],
              exports = [
                  "//3rdparty/jvm/org/typelevel:cats_core",
                  "//3rdparty/jvm/org/typelevel:paiges_core",
              ],
              visibility = ["//visibility:public"])

scala_library(name = "commands",
              srcs = ["Commands.scala"],
              deps = ["//3rdparty/jvm/com/monovore:decline", ":depsmodel"],
              exports = ["//3rdparty/jvm/com/monovore:decline", ":depsmodel"],
              )

scala_library(name = "graph",
              srcs = ["Graph.scala"],
              deps = ["//3rdparty/jvm/org/typelevel:paiges_core"],
              exports = ["//3rdparty/jvm/org/typelevel:paiges_core"],
              visibility = ["//visibility:public"])

scala_repl(name = "repl",
           deps = [":graph"])

scala_library(name = "resolver",
              srcs = ["Resolver.scala"],
              deps = [
                  "//3rdparty/jvm/org/typelevel:cats_core",
                  ":depsmodel",
                  ":graph",
              ],
              visibility = ["//visibility:public"])

scala_library(
    name = "coursier_resolver",
    srcs = ["CoursierResolver.scala"],
    deps = [
        "//3rdparty/jvm/io/get_coursier:coursier",
        "//3rdparty/jvm/io/get_coursier:coursier_cache",
        "//3rdparty/jvm/org/typelevel:cats_core",
        "//3rdparty/jvm/org/slf4j:slf4j_api",
        ":resolver",
        ":depsmodel",
        ":graph",
        ],
    visibility = ["//visibility:public"])

scala_library(name = "aether_resolver",
              srcs = ["AetherResolver.scala"],
              deps = [
                  "//3rdparty/jvm/org/typelevel:cats_core",
                  "//3rdparty/jvm/org/apache/maven:maven_aether_provider",
                  "//3rdparty/jvm/org/eclipse/aether:aether_api",
                  "//3rdparty/jvm/org/eclipse/aether:aether_spi",
                  "//3rdparty/jvm/org/eclipse/aether:aether_connector_basic",
                  "//3rdparty/jvm/org/eclipse/aether:aether_impl",
                  "//3rdparty/jvm/org/eclipse/aether:aether_transport_file",
                  "//3rdparty/jvm/org/eclipse/aether:aether_transport_http",
                  "//3rdparty/jvm/org/slf4j:slf4j_api",
                  ":depsmodel",
                  ":graph",
                  ":resolver",
                  ],
              visibility = ["//visibility:public"])

scala_library(name = "io",
              srcs = ["IO.scala"],
              deps = [
                  "//3rdparty/jvm/org/typelevel:cats_core",
                  "//3rdparty/jvm/org/typelevel:cats_free",
                  "//3rdparty/jvm/org/slf4j:slf4j_api",
              ],
              exports = [
                  "//3rdparty/jvm/org/typelevel:cats_free"
              ],
              visibility = ["//visibility:public"])

scala_library(name = "normalizer",
              srcs = ["Normalizer.scala"],
              deps = [":depsmodel",
                      ":graph",
                      "//3rdparty/jvm/org/typelevel:paiges_core",
                      ],
              visibility = ["//visibility:public"])

scala_library(name = "writer",
              srcs = ["Writer.scala", "Label.scala", "Target.scala"],
              deps = [":depsmodel",
                      ":graph",
                      ":io",
                      "//3rdparty/jvm/org/typelevel:cats_core",
                      "//3rdparty/jvm/org/typelevel:cats_free",
                      "//3rdparty/jvm/org/slf4j:slf4j_api",
                      "//3rdparty/jvm/org/typelevel:paiges_core",
                      ],
              visibility = ["//visibility:public"],
              scalacopts = ["-Ypartial-unification"])

scala_library(name = "makedeps",
             srcs = ["MakeDeps.scala", "FormatDeps.scala", "MergeDeps.scala"],
             deps = [
                 "//3rdparty/jvm/io/get_coursier:coursier",
                 "//3rdparty/jvm/io/get_coursier:coursier_cache",
                 "//3rdparty/jvm/io/circe:circe_core",
                 "//3rdparty/jvm/io/circe:circe_jawn",
                 "//3rdparty/jvm/org/eclipse/aether:aether_api",
                 "//3rdparty/jvm/org/typelevel:cats_core",
                 "//3rdparty/jvm/org/typelevel:cats_free",
                 "//3rdparty/jvm/org/slf4j:slf4j_api",
                 ":circeyaml",
                 ":commands",
                 ":decoders",
                 ":depsmodel",
                 ":graph",
                 ":io",
                 ":normalizer",
                 ":aether_resolver",
                 ":coursier_resolver",
                 ":resolver",
                 ":writer",
             ],
             visibility = ["//visibility:public"])

scala_binary(name = "parseproject",
             srcs = ["ParseProject.scala"],
             deps = [
                 ":makedeps",
                 ":commands",
                 "//3rdparty/jvm/org/slf4j:slf4j_simple",
             ],
             main_class = "com.github.johnynek.bazel_deps.ParseProject",
             visibility = ["//visibility:public"])

scala_library(name = "circeyaml",
              srcs = ["CirceYaml.scala"],
              deps = [
                  "//3rdparty/jvm/org/typelevel:cats_core",
                  "//3rdparty/jvm/io/circe:circe_core",
                  "//3rdparty/jvm/io/circe:circe_jackson25",
                  "//3rdparty/jvm/com/fasterxml/jackson/core:jackson_core",
                  "//3rdparty/jvm/com/fasterxml/jackson/core:jackson_databind",
                  "//3rdparty/jvm/com/fasterxml/jackson/dataformat:jackson_dataformat_yaml",
              ],
              visibility = ["//visibility:public"])

scala_library(name = "decoders",
              srcs = ["Decoders.scala"],
              deps = [
                  ":depsmodel",
                  "//3rdparty/jvm/io/circe:circe_core",
                  "//3rdparty/jvm/io/circe:circe_generic",
              ],
              visibility = ["//visibility:public"])

