load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
scala_import(
    name = "scalatest",
    exports = [
        "//3rdparty/jvm/org/scalactic:scalactic"
    ],
    jars = [
        "//external:jar/org/scalatest/scalatest_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang/modules:scala_parser_combinators",
        "//3rdparty/jvm/org/scala_lang/modules:scala_xml",
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_lang:scala_reflect"
    ],
    visibility = [
        "//visibility:public"
    ]
)


