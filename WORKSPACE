workspace(name = "com_github_johnynek_bazel_deps")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl",
     "git_repository", "new_git_repository")

git_repository(
    name = "io_bazel_rules_scala",
    remote = "https://github.com/bazelbuild/rules_scala",
    commit = "dc5a793e8e643a6aa4889b14e08ce9554ac667ec" # HEAD as of 2019-05-13, update this as needed
)

http_archive(
    name = "com_google_protobuf",
    sha256 = "1e622ce4b84b88b6d2cdf1db38d1a634fe2392d74f0b7b74ff98f3a51838ee53",
    urls = ["https://github.com/protocolbuffers/protobuf/archive/v3.8.0.zip"],
    strip_prefix = "protobuf-3.8.0",
)

http_archive(
    name = "bazel_skylib",
    sha256 = "7832382668c6dde9f57e18923763a24f9087cac66a50fbcc5afca848d03f2aa1",
    strip_prefix = "bazel-skylib-b113ed5d05ccddee3093bb157b9b02ab963c1c32",
    urls = ["https://github.com/bazelbuild/bazel-skylib/archive/b113ed5d05ccddee3093bb157b9b02ab963c1c32.tar.gz"],
)

load("@com_google_protobuf//:protobuf_deps.bzl", "protobuf_deps")

protobuf_deps()


load("@io_bazel_rules_scala//scala:scala.bzl", "scala_repositories")
scala_repositories()
register_toolchains("//:scala_toolchain")

load("//3rdparty:workspace.bzl", "maven_dependencies")

maven_dependencies()


load("//3rdparty:target_file.bzl", "build_external_workspace")
build_external_workspace(name = "third_party")

bind(name = 'io_bazel_rules_scala/dependency/scalatest/scalatest', actual = '//3rdparty/jvm/org/scalatest')
