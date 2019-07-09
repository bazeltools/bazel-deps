load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
scala_import(
    name = "scalacheck",
    jars = [
        "//external:jar/org/scalacheck/scalacheck_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_sbt:test_interface"
    ],
    visibility = [
        "//visibility:public"
    ]
)


