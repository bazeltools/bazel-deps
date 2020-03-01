set -e
BAZEL_PLATFORM_NAME=$1
cd $GITHUB_WORKSPACE

wget "https://github.com/bazelbuild/bazel/releases/download/2.1.1/bazel-2.1.1-installer-${BAZEL_PLATFORM_NAME}-x86_64.sh"
sha256sum -c ci_scripts/.bazel-installer-${BAZEL_PLATFORM_NAME}-x86_64.sh.sha256
chmod +x bazel-2.1.1-installer-${BAZEL_PLATFORM_NAME}-x86_64.sh
./bazel-2.1.1-installer-${BAZEL_PLATFORM_NAME}-x86_64.sh --user
cp .bazelrc.travis .bazelrc


