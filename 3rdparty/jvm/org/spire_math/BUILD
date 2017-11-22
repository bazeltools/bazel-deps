load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
java_library(
    name = "jawn_parser_2_11",
    exports = [
        "//external:jar/org/spire_math/jawn_parser_2_11"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "kind_projector",
    jars = [
        "//external:jar/org/spire_math/kind_projector_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_compiler",
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)


