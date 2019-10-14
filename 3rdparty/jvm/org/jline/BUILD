load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
java_library(
    name = "jline_terminal",
    exports = [
        "//external:jar/org/jline/jline_terminal"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



java_library(
    name = "jline_terminal_jansi",
    exports = [
        "//external:jar/org/jline/jline_terminal_jansi"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/fusesource/jansi:jansi",
        ":jline_terminal"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)


