# Top level Bazel run alias for parseproject
alias(
    name = "parse",
    actual = "//src/scala/com/github/johnynek/bazel_deps:parseproject"
)

load("@io_bazel_rules_scala//scala:scala_toolchain.bzl", "scala_toolchain")

scala_toolchain(
    name = "scala_toolchain_impl",
    scalacopts = ["-Xmax-classfile-name", "140"],
    visibility = ["//visibility:public"]
)

toolchain(
    name = "scala_toolchain",
    toolchain = "scala_toolchain_impl",
    toolchain_type = "@io_bazel_rules_scala//scala:toolchain_type",
    visibility = ["//visibility:public"]
)
