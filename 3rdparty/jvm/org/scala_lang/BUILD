load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
scala_library(
    name = "scala_compiler",
    exports = [
        "//external:jar/org/scala_lang/scala_compiler"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang/modules:scala_parser_combinators",
        "//3rdparty/jvm/org/scala_lang/modules:scala_xml",
        ":scala_reflect"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_library(
    name = "scala_library",
    exports = [
        "//3rdparty/manual:scala_library_file"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_library(
    name = "scala_reflect",
    exports = [
        "//3rdparty/manual:scala_reflect_file"
    ],
    visibility = [
        "//visibility:public"
    ]
)


