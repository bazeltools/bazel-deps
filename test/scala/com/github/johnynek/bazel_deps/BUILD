scala_library(name = "gencheats",
              srcs = ["GenCheats.scala"],
              deps = [
                  "//3rdparty/jvm/org/scalacheck",
                  "//3rdparty/jvm/org/typelevel:cats_core",
                  ])

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
               ":gencheats",
               "//src/scala/com/github/johnynek/bazel_deps:depsmodel",
               "//src/scala/com/github/johnynek/bazel_deps:graph",
               "//src/scala/com/github/johnynek/bazel_deps:normalizer",
               "//3rdparty/jvm/org/scalacheck"])

scala_library(name = "modelgen",
           srcs = ["ModelGenerators.scala"],
           deps = ["//src/scala/com/github/johnynek/bazel_deps:depsmodel",
                   "//3rdparty/jvm/org/typelevel:paiges_core",
                   "//3rdparty/jvm/org/scalacheck"])


scala_library(name = "parsetestutil",
           srcs = ["ParseTestUtil.scala"],
           deps = [
               ":modelgen",
               "//3rdparty/jvm/org/scalatest",
               "//3rdparty/jvm/org/scalacheck",
               "//3rdparty/jvm/org/typelevel:cats_core",
               "//3rdparty/jvm/io/circe:circe_core",
               "//src/scala/com/github/johnynek/bazel_deps:circeyaml",
               "//src/scala/com/github/johnynek/bazel_deps:decoders",
               "//src/scala/com/github/johnynek/bazel_deps:depsmodel",
               ])

scala_test(name = "parsetest",
           srcs = ["ParseTest.scala"],
           deps = [
               ":modelgen",
               ":parsetestutil",
               "//3rdparty/jvm/org/scalacheck",
               "//3rdparty/jvm/org/typelevel:cats_core",
               "//3rdparty/jvm/io/circe:circe_core",
               "//src/scala/com/github/johnynek/bazel_deps:circeyaml",
               "//src/scala/com/github/johnynek/bazel_deps:decoders",
               "//src/scala/com/github/johnynek/bazel_deps:depsmodel",
               ])

scala_test(name = "parsegenerateddoctest",
           srcs = ["ParseGeneratedDocTest.scala"],
           jvm_flags = ["-Xmx1000M", "-Xms1000M"],
           deps = [
               ":modelgen",
               ":parsetestutil",
               "//3rdparty/jvm/org/scalacheck",
               "//3rdparty/jvm/org/typelevel:cats_core",
               "//3rdparty/jvm/io/circe:circe_core",
               "//src/scala/com/github/johnynek/bazel_deps:circeyaml",
               "//src/scala/com/github/johnynek/bazel_deps:decoders",
               "//src/scala/com/github/johnynek/bazel_deps:depsmodel",
               ])

scala_test(name = "parsetestcases",
           srcs = ["ParseTestCasesTest.scala"],
           deps = [
               ":modelgen",
               ":parsetestutil",
               "//3rdparty/jvm/org/scalacheck",
               "//3rdparty/jvm/org/typelevel:cats_core",
               "//3rdparty/jvm/io/circe:circe_core",
               "//src/scala/com/github/johnynek/bazel_deps:circeyaml",
               "//src/scala/com/github/johnynek/bazel_deps:decoders",
               "//src/scala/com/github/johnynek/bazel_deps:depsmodel",
               ])

scala_test(name = "coursier_test",
           srcs = ["CoursierTest.scala"],
           deps = [
               "//3rdparty/jvm/io/circe:circe_core",
               "//src/scala/com/github/johnynek/bazel_deps:writer",
               "//src/scala/com/github/johnynek/bazel_deps:circeyaml",
               "//src/scala/com/github/johnynek/bazel_deps:decoders",
               "//src/scala/com/github/johnynek/bazel_deps:depsmodel",
               "//src/scala/com/github/johnynek/bazel_deps:makedeps",
               "//src/scala/com/github/johnynek/bazel_deps:graph",
               ])

test_suite(name = "all_tests")