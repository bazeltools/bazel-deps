scala_library(name = "maven",
              srcs = ["Tool.scala"],
              deps = [
                  "//3rdparty/jvm/io/circe:circe_jawn_2_11",
                  "//3rdparty/jvm/io/circe:circe_core",
                  "//3rdparty/jvm/org/scala_lang/modules:scala_xml",
                  "//3rdparty/jvm/org/typelevel:cats_core",
                  "//src/scala/com/github/johnynek/bazel_deps:circeyaml",
                  "//src/scala/com/github/johnynek/bazel_deps:depsmodel",
                  "//src/scala/com/github/johnynek/bazel_deps:io",
                  "//src/scala/com/github/johnynek/bazel_deps:decoders",
                  "//src/scala/com/github/johnynek/bazel_deps:writer",
              ],
              visibility = ["//visibility:public"])

scala_repl(name = "repl",
           deps = [":maven"])

scala_binary(
    name = "maven_tool",
    main_class = "com.github.johnynek.bazel_deps.maven.Tool",
    deps = [":maven"]
)
