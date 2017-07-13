scala_test(name = "modeltest",
           srcs = ["ModelTest.scala"],
           size = "small",
           deps = ["//src/scala/com/github/johnynek/bazel_deps:depsmodel"])

scala_test(name = "graphtest",
           srcs = ["GraphTest.scala"],
           size = "small",
           deps = ["//src/scala/com/github/johnynek/bazel_deps:graph",
                   "//3rdparty/jvm/org/scalacheck"])

scala_test(name = "normalizertest",
           srcs = ["NormalizerTest.scala"],
           size = "small",
           deps = [
               "//src/scala/com/github/johnynek/bazel_deps:depsmodel",
               "//src/scala/com/github/johnynek/bazel_deps:graph",
               "//src/scala/com/github/johnynek/bazel_deps:normalizer",
               "//3rdparty/jvm/org/scalacheck"])

scala_library(name = "modelgen",
           srcs = ["ModelGenerators.scala"],
           deps = ["//src/scala/com/github/johnynek/bazel_deps:depsmodel",
                   "@org_typelevel_paiges//:paiges",
                   "//3rdparty/jvm/org/scalacheck"])

scala_test(name = "parsetest",
           srcs = ["ParseTest.scala"],
           jvm_flags = ["-Xmx1000M", "-Xms1000M"],
           deps = [
               ":modelgen",
               "//3rdparty/jvm/org/scalacheck",
               "//3rdparty/jvm/org/typelevel:cats_core",
               "//3rdparty/jvm/io/circe:circe_core",
               "//src/scala/com/github/johnynek/bazel_deps:circeyaml",
               "//src/scala/com/github/johnynek/bazel_deps:decoders",
               "//src/scala/com/github/johnynek/bazel_deps:depsmodel",
               ])
