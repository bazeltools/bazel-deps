workspace(name = "com_github_pgr0ss_bazel_deps")

git_repository(
    name = "io_bazel_rules_scala",
    remote = "git://github.com/bazelbuild/rules_scala",
    commit = "8dd28efcd80a201460fc47d297919803219226e0", # update this as needed
)
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_repositories")
scala_repositories()

git_repository(
    name = "io_bazel",
    remote = "git://github.com/bazelbuild/bazel.git",
    commit = "0.3.1",
)

load("//3rdparty:workspace.bzl", "maven_dependencies")
load("//3rdparty:load.bzl", "declare_maven")

maven_dependencies(declare_maven)
