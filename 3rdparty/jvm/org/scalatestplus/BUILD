load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
scala_import(
    name = "scalacheck_1_17",
    jars = [
        "//external:jar/org/scalatestplus/scalacheck_1_17_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scalacheck:scalacheck",
        "//3rdparty/jvm/org/scalatest:scalatest_core"
    ],
    visibility = [
        "//visibility:public"
    ]
)


