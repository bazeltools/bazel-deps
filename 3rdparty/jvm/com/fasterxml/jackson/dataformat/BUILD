load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
java_library(
    name = "jackson_dataformat_cbor",
    exports = [
        "//external:jar/com/fasterxml/jackson/dataformat/jackson_dataformat_cbor"
    ],
    runtime_deps = [
        "//3rdparty/jvm/com/fasterxml/jackson/core:jackson_core"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



java_library(
    name = "jackson_dataformat_yaml",
    exports = [
        "//external:jar/com/fasterxml/jackson/dataformat/jackson_dataformat_yaml"
    ],
    runtime_deps = [
        "//3rdparty/jvm/com/fasterxml/jackson/core:jackson_core",
        "//3rdparty/jvm/com/fasterxml/jackson/core:jackson_databind",
        "//3rdparty/jvm/org/yaml:snakeyaml"
    ],
    visibility = [
        "//visibility:public"
    ]
)


