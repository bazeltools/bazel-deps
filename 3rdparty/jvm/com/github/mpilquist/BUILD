load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
scala_import(
    name = "simulacrum",
    exports = [
        "//3rdparty/jvm/org/typelevel:macro_compat"
    ],
    jars = [
        "//external:jar/com/github/mpilquist/simulacrum_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)


