load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
scala_import(
    name = "jawn_parser",
    jars = [
        "//external:jar/org/spire_math/jawn_parser_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
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


