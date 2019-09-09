workspace(name = "com_github_johnynek_bazel_deps")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl",
     "git_repository", "new_git_repository")

 # TODO(Jonathon): This pre-fetching of zlib, and reworking of com_google_protobuf
 # is a temporary hack (found in https://github.com/bazelbuild/rules_scala/issues/726)
 # that is used to avoid compiling Protobuf from source. Need this until our C++ toolchain is fixed because
 # right now we can't compile Protobuf on our Mac Pros.

PROTOBUF_JAVA_VERSION = "3.7.1"

http_archive(
    name = "zlib",
    build_file = "@bazel_tools//third_party/zlib:BUILD",
    sha256 = "c3e5e9fdd5004dcb542feda5ee4f0ff0744628baf8ed2dd5d66f8ca1197cb1a1",
    strip_prefix = "zlib-1.2.11",
    urls = ["https://zlib.net/zlib-1.2.11.tar.gz"],
)

http_archive(
    name = "com_google_protobuf",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
    name = "meta",
    srcs = glob([
        "META-INF/**/*",
    ])
)

filegroup(
    name = "classes",
    srcs = glob([
        "com/**/*",
        "google/**/*",
    ])
)

genrule(
    name = "protobuf_java",
    srcs = [":meta", ":classes"],
    outs = ["myjar.jar"],
    toolchains = ["@bazel_tools//tools/jdk:current_java_runtime"],
    cmd = "mkdir -p META-INF && mkdir -p com/google/protobuf && cp -r $(locations :meta) META-INF && cp -r $(locations :classes) com/google/protobuf && $(JAVABASE)/bin/jar cfM $@ META-INF com",
)
    """,
    sha256 = "22779eacfe0f33b9e8ceffa3cdef5935cae4e53f736e027d912b707522fea645",
    url = "http://central.maven.org/maven2/com/google/protobuf/protobuf-java/%s/protobuf-java-%s.jar" % (PROTOBUF_JAVA_VERSION, PROTOBUF_JAVA_VERSION),
)


git_repository(
    name = "io_bazel_rules_scala",
    remote = "https://github.com/bazelbuild/rules_scala",
    commit = "dc5a793e8e643a6aa4889b14e08ce9554ac667ec" # HEAD as of 2019-05-13, update this as needed
)


load("@io_bazel_rules_scala//scala:scala.bzl", "scala_repositories")
scala_repositories()
register_toolchains("//:scala_toolchain")

load("//3rdparty:workspace.bzl", "maven_dependencies")

maven_dependencies()


load("//3rdparty:target_file.bzl", "build_external_workspace")
build_external_workspace(name = "third_party")

bind(name = 'io_bazel_rules_scala/dependency/scalatest/scalatest', actual = '//3rdparty/jvm/org/scalatest')
