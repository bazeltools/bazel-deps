load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
java_library(
    name = "junit_interface",
    exports = [
        "//external:jar/org/scalameta/junit_interface"
    ],
    runtime_deps = [
        "//3rdparty/jvm/junit:junit",
        "//3rdparty/jvm/org/scala_sbt:test_interface"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "munit",
    jars = [
        "//external:jar/org/scalameta/munit_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/junit:junit",
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":junit_interface"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "munit_scalacheck",
    jars = [
        "//external:jar/org/scalameta/munit_scalacheck_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scalacheck:scalacheck",
        ":munit"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)


