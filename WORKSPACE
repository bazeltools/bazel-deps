workspace(name = "com_github_johnynek_bazel_deps")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl",
     "git_repository", "new_git_repository")


http_archive(
    name = "zlib",
    build_file = "@com_google_protobuf//:third_party/zlib.BUILD",
    sha256 = "c3e5e9fdd5004dcb542feda5ee4f0ff0744628baf8ed2dd5d66f8ca1197cb1a1",
    strip_prefix = "zlib-1.2.11",
    urls = [
        "https://mirror.bazel.build/zlib.net/zlib-1.2.11.tar.gz",
        "https://zlib.net/zlib-1.2.11.tar.gz",
    ],
)

git_repository(
    name = "io_bazel_rules_scala",
    remote = "https://github.com/bazelbuild/rules_scala",
    commit = "a676633dc14d8239569affb2acafbef255df3480" # HEAD as of 2020-01-15, update this as needed
)

http_archive(
    name = "com_google_protobuf",
    sha256 = "662879e41508a5ecce3be2c65563a8fac3301a48adef3113913ec4010f405a33",
    strip_prefix = "protobuf-3.20.1",
    urls = ["https://github.com/protocolbuffers/protobuf/archive/v3.20.1.zip"],
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
scala_repositories((
    "2.12.10",
    {
       "scala_compiler": "cedc3b9c39d215a9a3ffc0cc75a1d784b51e9edc7f13051a1b4ad5ae22cfbc0c",
       "scala_library": "0a57044d10895f8d3dd66ad4286891f607169d948845ac51e17b4c1cf0ab569d",
       "scala_reflect": "56b609e1bab9144fb51525bfa01ccd72028154fc40a58685a1e9adcbe7835730"
    }
))

register_toolchains("//:scala_toolchain")

load("//3rdparty:workspace.bzl", "maven_dependencies")

maven_dependencies()


load("//3rdparty:target_file.bzl", "build_external_workspace")
build_external_workspace(name = "third_party")
bind(name = 'io_bazel_rules_scala/dependency/scalatest/scalatest', actual = '//3rdparty/jvm/org/scalatest')
