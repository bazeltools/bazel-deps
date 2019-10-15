load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
scala_import(
    name = "argonaut",
    jars = [
        "//external:jar/io/argonaut/argonaut_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_reflect"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)


