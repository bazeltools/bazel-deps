load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
scala_import(
    name = "shapeless",
    jars = [
        "//external:jar/com/chuusai/shapeless_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/typelevel:macro_compat"
    ],
    visibility = [
        "//visibility:public"
    ]
)


