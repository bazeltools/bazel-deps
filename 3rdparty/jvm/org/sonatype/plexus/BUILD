load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
java_library(
    name = "plexus_cipher",
    exports = [
        "//external:jar/org/sonatype/plexus/plexus_cipher"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "plexus_sec_dispatcher",
    exports = [
        "//external:jar/org/sonatype/plexus/plexus_sec_dispatcher"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/codehaus/plexus:plexus_utils",
        ":plexus_cipher"
    ],
    visibility = [
        "//visibility:public"
    ]
)


