load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
java_library(
    name = "httpclient",
    exports = [
        "//external:jar/org/apache/httpcomponents/httpclient"
    ],
    runtime_deps = [
        "//3rdparty/jvm/commons_codec:commons_codec",
        ":httpcore"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



java_library(
    name = "httpcore",
    exports = [
        "//external:jar/org/apache/httpcomponents/httpcore"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)


