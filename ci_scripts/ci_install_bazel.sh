set -e
cd $GITHUB_WORKSPACE

cp ci_scripts/bootstrapping_bazel bazel
cp ci_scripts/bootstrapping_bazel tools/bazel

./bazel
cp .bazelrc.travis .bazelrc


