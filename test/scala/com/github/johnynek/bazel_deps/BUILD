scala_test(name = "modeltest",
           srcs = ["ModelTest.scala"],
           deps = ["//src/scala/com/github/johnynek/bazel_deps:depsmodel"])

scala_test(name = "graphtest",
           srcs = ["GraphTest.scala"],
           deps = ["//src/scala/com/github/johnynek/bazel_deps:graph",
                   "//3rdparty/jvm/org/scalacheck"])

scala_test(name = "parsetest",
           srcs = ["ParseTest.scala"],
           deps = [
               "//3rdparty/jvm/org/typelevel:cats_core_2_11",
               "//3rdparty/jvm/io/circe:circe_core",
               "//src/scala/com/github/johnynek/bazel_deps:circeyaml",
               "//src/scala/com/github/johnynek/bazel_deps:decoders",
               "//src/scala/com/github/johnynek/bazel_deps:depsmodel",
               ])
