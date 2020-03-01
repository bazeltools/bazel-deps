set -e
bazel build src/scala/com/github/johnynek/bazel_deps/parseproject_deploy.jar
cp bazel-bin/src/scala/com/github/johnynek/bazel_deps/parseproject_deploy.jar bazel_deps.jar
