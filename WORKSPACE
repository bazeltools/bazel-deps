workspace(name = "com_github_johnynek_bazel_deps")

load("@bazel_tools//tools/build_defs/repo:git.bzl",
     "git_repository", "new_git_repository")

git_repository(
    name = "io_bazel_rules_scala",
    remote = "git://github.com/bazelbuild/rules_scala",
    commit = "5874a2441596fe9a0bf80e167a4d7edd945c221e" # update this as needed
)

load("@io_bazel_rules_scala//scala:toolchains.bzl", "scala_register_toolchains")
scala_register_toolchains()

load("@io_bazel_rules_scala//scala:scala.bzl", "scala_repositories")
scala_repositories()
load("@io_bazel_rules_scala//scala:toolchains.bzl", "scala_register_toolchains")
scala_register_toolchains()

load("//3rdparty:workspace.bzl", "maven_dependencies")

maven_dependencies()

bind(name = 'io_bazel_rules_scala/dependency/scalatest/scalatest', actual = '//3rdparty/jvm/org/scalatest')
