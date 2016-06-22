#!/bin/bash

ABSOLUTE_PATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

PATH_TO_JAR=$ABSOLUTE_PATH/bazel-bin/src/scala/com/github/johnynek/bazel_deps/parseproject_deploy.jar

if [ ! -e $PATH_TO_JAR ]
then
  echo "could not find $PATH_TO_JAR"
  echo "perhaps build it with:"
  echo "  bazel build src/scala/com/github/johnynek/bazel_deps/parseproject_deploy.jar"
  exit 1
fi

java -cp $PATH_TO_JAR com.github.johnynek.bazel_deps.ParseProject $@
