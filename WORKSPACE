workspace(name = "com_github_johnynek_bazel_deps")

load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

git_repository(
    name = "io_bazel_rules_scala",
    remote = "git://github.com/bazelbuild/rules_scala",
    commit = "4afc7abc73e4fb688e7c95e7aec43005feb44b0b" # update this as needed
)

load("@io_bazel_rules_scala//scala:toolchains.bzl", "scala_register_toolchains")
scala_register_toolchains()

load("@io_bazel_rules_scala//scala:scala.bzl", "scala_repositories")
scala_repositories()

load("//3rdparty:workspace.bzl", "maven_dependencies")

maven_dependencies()

new_git_repository(
    name = "org_typelevel_paiges",
    remote = "git://github.com/typelevel/paiges",
    commit = "8df2440d3bb4260b0772d6971e14c9d9322b077d",
    # inconsistency in how we refer to build paths in new_native/new git
    build_file = "3rdparty/manual/BUILD.paiges",
    # use target: "@org_typelevel_paiges//:paiges"
)

new_git_repository(
    name = "com_monovore_decline",
    remote = "git://github.com/bkirwi/decline",
    # this is 0.2.2
    commit = "b3dd1442923949f1fd2822b266f2e3626919f2c6",
    # inconsistency in how we refer to build paths in new_native/new git
    build_file = "3rdparty/manual/BUILD.decline",
    # use target: "@org_typelevel_paiges//:decline"
)

bind(name = 'io_bazel_rules_scala/dependency/scalatest/scalatest', actual = '//3rdparty/jvm/org/scalatest')
