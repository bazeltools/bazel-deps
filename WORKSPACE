workspace(name = "com_github_pgr0ss_bazel_deps")

git_repository(
    name = "io_bazel_rules_scala",
    remote = "git://github.com/bazelbuild/rules_scala",
    commit = "ec7b354dc17de23d5d3e001f0b18d901b3c40b99", # update this as needed
)
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_repositories")
scala_repositories()

load("//3rdparty:workspace.bzl", "maven_dependencies")
load("//3rdparty:load.bzl", "declare_maven")

maven_dependencies(declare_maven)
