workspace(name = "com_github_johnynek_bazel_deps")

git_repository(
    name = "io_bazel_rules_scala",
    remote = "git://github.com/bazelbuild/rules_scala",
    commit = "690cf39eba8eccf3c6e21ca77d8a4e1710aa3629" # update this as needed
)
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_repositories")
scala_repositories()

load("//3rdparty:workspace.bzl", "maven_dependencies")
load("//3rdparty:load.bzl", "declare_maven")

maven_dependencies(declare_maven)

new_git_repository(
    name = "com_github_johnynek_paiges",
    remote = "git://github.com/johnynek/paiges",
    commit = "0c74df95ac82674f8a369e293df4daf7436b44d8",
    # inconsistency in how we refer to build paths in new_native/new git
    build_file = "3rdparty/manual/BUILD.paiges",
    # use target: "@com_github_johnynek_paiges//:paiges"
)
