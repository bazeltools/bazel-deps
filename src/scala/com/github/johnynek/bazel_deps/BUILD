scala_library(name = "depsmodel",
              srcs = ["DepsModel.scala"],
              visibility = ["//visibility:public"])

scala_library(name = "graph",
              srcs = ["Graph.scala"],
              visibility = ["//visibility:public"])

scala_library(name = "resolver",
              srcs = ["Resolver.scala"],
              deps = [
                  "//3rdparty/jvm/org/apache/maven:maven_aether_provider",
                  "//3rdparty/jvm/org/eclipse/aether:aether_api",
                  "//3rdparty/jvm/org/eclipse/aether:aether_connector_basic",
                  "//3rdparty/jvm/org/eclipse/aether:aether_impl",
                  "//3rdparty/jvm/org/eclipse/aether:aether_transport_file",
                  "//3rdparty/jvm/org/eclipse/aether:aether_transport_http",
                  ":depsmodel",
                  ":graph",
                  ],
              visibility = ["//visibility:public"])

scala_library(name = "normalizer",
              srcs = ["Normalizer.scala"],
              deps = [":depsmodel",
                      ":graph",
                      ],
              visibility = ["//visibility:public"])

scala_library(name = "writer",
              srcs = ["Writer.scala"],
              deps = [":depsmodel",
                      ":graph",
                      ],
              visibility = ["//visibility:public"])

scala_library(name = "makedeps",
             srcs = ["MakeDeps.scala"],
             deps = [
                 ":resolver",
                 ":graph",
                 ":depsmodel",
                 ":normalizer",
                 ":writer",
                 "//3rdparty/jvm/org/eclipse/aether:aether_api",
             ],
             visibility = ["//visibility:public"])

scala_binary(name = "parseproject",
             srcs = ["ParseProject.scala"],
             deps = [
                 ":makedeps",
                 ":depsmodel",
                 ":circeyaml",
                 ":decoders",
                 "//3rdparty/jvm/io/circe:circe_core",
                 "//3rdparty/jvm/org/typelevel:cats_core_2_11",
                 "//3rdparty/jvm/io/circe:circe_jawn_2_11",
                 ],
             main_class = "com.github.johnynek.bazel_deps.ParseProject",
             visibility = ["//visibility:public"])

scala_binary(name = "projectmodel",
             srcs = ["ProjectModel.scala"],
             deps = [":makedeps", ":depsmodel" ],
             main_class = "com.github.johnynek.bazel_deps.ProjectModel",
             visibility = ["//visibility:public"])

scala_library(name = "circeyaml",
              srcs = ["CirceYaml.scala"],
              deps = [
                  "//3rdparty/jvm/io/circe:circe_core",
                  "//3rdparty/jvm/io/circe:circe_jackson",
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
