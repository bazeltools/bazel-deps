load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
java_library(
    name = "cdi_api",
    exports = [
        "//external:jar/javax/enterprise/cdi_api"
    ],
    runtime_deps = [
        "//3rdparty/jvm/javax/annotation:jsr250_api"
    ],
    visibility = [
        "//visibility:public"
    ]
)


