scala_library(name = "depsmodel",
              srcs = ["DepsModel.scala"],
              visibility = ["//visibility:public"])

scala_library(name = "graph",
              srcs = ["Graph.scala"],
              visibility = ["//visibility:public"])

scala_library(name = "resolver",
              srcs = ["Resolver.scala"],
              deps = [
                  "//3rdparty:aether-api",
                  "//3rdparty:aether-connector-basic",
                  "//3rdparty:aether-impl",
                  "//3rdparty:aether-transport-file",
                  "//3rdparty:aether-transport-http",
                  "//3rdparty:maven-aether-provider",
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
                 "//3rdparty:aether-api",
             ],
             main_class = "com.github.johnynek.bazel_deps.MakeDeps",
             visibility = ["//visibility:public"])
