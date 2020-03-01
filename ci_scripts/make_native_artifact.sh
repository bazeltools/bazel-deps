set -eux
# NOTE(olafur): for some reason `jabba use ...` doesn't seem to work on GH Actions
export JAVA_HOME=$(jabba which --home graal-custom@20.0)
export PATH=$JAVA_HOME/bin:$PATH
echo $JAVA_HOME
which gu
gu install native-image

cd $GITHUB_WORKSPACE
native-image --no-fallback -jar bazel-deps.jar

# ensure it actually works!
./bazel-deps generate --repo-root `pwd` --sha-file 3rdparty/workspace.bzl --deps dependencies.yaml --target-file 3rdparty/target_file.bzl --disable-3rdparty-in-repo
