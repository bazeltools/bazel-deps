set -eux
# NOTE(olafur): for some reason `jabba use ...` doesn't seem to work on GH Actions
export JAVA_HOME=$(jabba which --home graal-custom@22.3)
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
cp ../ci_scripts/resource-config.json .

native-image -H:+ReportUnsupportedElementsAtRuntime \
 --initialize-at-build-time \
 --no-server \
 --enable-http \
 --enable-https \
 -H:Log=registerResource: \
 -H:EnableURLProtocols=http,https \
 --enable-all-security-services \
 -H:ReflectionConfigurationFiles=reflection.json \
 -H:ResourceConfigurationFiles=resource-config.json \
 --allow-incomplete-classpath \
 -H:+ReportExceptionStackTraces \
 --no-fallback \
 -H:IncludeResources='.*bzl$' \
 -cp bazel-deps.jar \
 -jar bazel-deps.jar

 cd ..
 rm -f bazel-deps
 mv native_image_working_directory/bazel-deps .
 rm -rf native_image_working_directory

# ensure it actually works!
./bazel-deps generate --repo-root `pwd` --resolved-output 3rdparty/resolved-deps.json --deps dependencies.yaml
