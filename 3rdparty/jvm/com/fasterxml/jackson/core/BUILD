load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
java_library(
    name = "jackson_annotations",
    exports = [
        "//external:jar/com/fasterxml/jackson/core/jackson_annotations"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



java_library(
    name = "jackson_core",
    exports = [
        "//external:jar/com/fasterxml/jackson/core/jackson_core"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "jackson_databind",
    exports = [
        "//external:jar/com/fasterxml/jackson/core/jackson_databind"
    ],
    runtime_deps = [
        ":jackson_annotations",
        ":jackson_core"
    ],
    visibility = [
        "//visibility:public"
    ]
)


