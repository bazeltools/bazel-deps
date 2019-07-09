load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
java_library(
    name = "cdi_api",
    exports = [
        "//external:jar/javax/enterprise/cdi_api"
    ],
    runtime_deps = [
        "//3rdparty/jvm/javax/annotation:jsr250_api",
        "//3rdparty/jvm/javax/inject:javax_inject"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)


