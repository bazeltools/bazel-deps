load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
java_library(
    name = "jcl_over_slf4j",
    exports = [
        "//external:jar/org/slf4j/jcl_over_slf4j"
    ],
    runtime_deps = [
        ":slf4j_api"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



java_library(
    name = "slf4j_api",
    exports = [
        "//external:jar/org/slf4j/slf4j_api"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "slf4j_simple",
    exports = [
        "//external:jar/org/slf4j/slf4j_simple"
    ],
    runtime_deps = [
        ":slf4j_api"
    ],
    visibility = [
        "//visibility:public"
    ]
)


