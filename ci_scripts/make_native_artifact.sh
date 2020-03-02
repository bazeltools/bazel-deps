set -eux
# NOTE(olafur): for some reason `jabba use ...` doesn't seem to work on GH Actions
export JAVA_HOME=$(jabba which --home graal-custom@20.0)
export PATH=$JAVA_HOME/bin:$PATH
echo $JAVA_HOME
which gu
gu install native-image

cd $GITHUB_WORKSPACE

rm -rf native_image_working_directory
mkdir native_image_working_directory
cd native_image_working_directory

mkdir templates/
cp ../src/scala/com/github/johnynek/bazel_deps/templates/* templates/
cp ../bazel-deps.jar .
cp ../ci_scripts/reflection.json .

native-image -H:+ReportUnsupportedElementsAtRuntime \
 --initialize-at-build-time \
 --no-server \
 --enable-http \
 --enable-https \
 -H:Log=registerResource: \
 -H:EnableURLProtocols=http,https \
 --enable-all-security-services \
 -H:ReflectionConfigurationFiles=reflection.json \
 --allow-incomplete-classpath \
 -H:+ReportExceptionStackTraces \
 --no-fallback \
 -H:IncludeResources='.*bzl$' \
 -jar bazel-deps.jar

 cd ..
 rm -f bazel-deps
 mv native_image_working_directory/bazel-deps .
 rm -rf native_image_working_directory

# ensure it actually works!
./bazel-deps generate --repo-root `pwd` --sha-file 3rdparty/workspace.bzl --deps dependencies.yaml --target-file 3rdparty/target_file.bzl --disable-3rdparty-in-repo
