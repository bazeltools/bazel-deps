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

scala_binary(name = "makedeps",
             srcs = ["MakeDeps.scala"],
             deps = [
                 ":resolver",
                 ":graph",
                 ":depsmodel",
                 ":normalizer",
                 ":writer",
                 "//3rdparty/jvm/org/eclipse/aether:aether_api",
             ],
             main_class = "com.github.johnynek.bazel_deps.MakeDeps",
             visibility = ["//visibility:public"])
