load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
java_library(
    name = "aws_java_sdk_core",
    exports = [
        "//external:jar/com/amazonaws/aws_java_sdk_core"
    ],
    runtime_deps = [
        "//3rdparty/jvm/com/fasterxml/jackson/core:jackson_databind",
        "//3rdparty/jvm/com/fasterxml/jackson/dataformat:jackson_dataformat_cbor",
        "//3rdparty/jvm/commons_logging:commons_logging",
        "//3rdparty/jvm/joda_time:joda_time",
        "//3rdparty/jvm/org/apache/httpcomponents:httpclient",
        "//3rdparty/jvm/software/amazon/ion:ion_java"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "aws_java_sdk_kms",
    exports = [
        "//external:jar/com/amazonaws/aws_java_sdk_kms"
    ],
    runtime_deps = [
        ":aws_java_sdk_core",
        ":jmespath_java"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



java_library(
    name = "aws_java_sdk_s3",
    exports = [
        "//external:jar/com/amazonaws/aws_java_sdk_s3"
    ],
    runtime_deps = [
        ":aws_java_sdk_core",
        ":aws_java_sdk_kms",
        ":jmespath_java"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "aws_java_sdk_sts",
    exports = [
        "//external:jar/com/amazonaws/aws_java_sdk_sts"
    ],
    runtime_deps = [
        ":aws_java_sdk_core",
        ":jmespath_java"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "jmespath_java",
    exports = [
        "//external:jar/com/amazonaws/jmespath_java"
    ],
    runtime_deps = [
        "//3rdparty/jvm/com/fasterxml/jackson/core:jackson_databind"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)


