workspace(name = "com_github_johnynek_bazel_deps")

git_repository(
    name = "io_bazel_rules_scala",
    remote = "git://github.com/bazelbuild/rules_scala",
    commit = "b51e54cf0a77f66c269c8c8fa24d62cac388337d" # update this as needed
)
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_repositories")
scala_repositories()

load("//3rdparty:workspace.bzl", "maven_dependencies")
load("//3rdparty:load.bzl", "declare_maven")

maven_dependencies(declare_maven)

new_git_repository(
    name = "com_github_johnynek_paiges",
    remote = "git://github.com/johnynek/paiges",
    commit = "8b3927f5c9c2a86011fd70217ee97d708268afe4",
    # inconsistency in how we refer to build paths in new_native/new git
    build_file = "3rdparty/manual/BUILD.paiges",
    # use target: "@com_github_johnynek_paiges//:paiges"
)

new_git_repository(
    name = "com_monovore_decline",
    remote = "git://github.com/bkirwi/decline",
    # this is 0.2.2
    commit = "b3dd1442923949f1fd2822b266f2e3626919f2c6",
    # inconsistency in how we refer to build paths in new_native/new git
    build_file = "3rdparty/manual/BUILD.decline",
    # use target: "@org_typelevel_paiges//:paiges"
)
