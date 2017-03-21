scala_library(name = "maven",
              srcs = ["Tool.scala"],
              deps = [
                  "//3rdparty/jvm/org/scala_lang/modules:scala_xml",
                  "//src/scala/com/github/johnynek/bazel_deps:depsmodel",
              ],
              visibility = ["//visibility:public"])

scala_repl(name = "repl",
           deps = [":maven"])

scala_binary(
    name = "maven_tool",
    main_class = "com.github.johnynek.bazel_deps.maven.Tool",
    deps = [":maven"]
)
