load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
scala_library(
    name = "scalactic",
    exports = [
        "//external:jar/org/scalactic/scalactic_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_lang:scala_reflect"
    ],
    visibility = [
        "//visibility:public"
    ]
)


