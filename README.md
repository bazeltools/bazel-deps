# bazel-deps

Generate [bazel](http://bazel.io/) dependencies for maven artifacts

## Usage

For now, you need to edit the MakeDeps.scala file to add your deps. This project is setup with:

```bash
bazel run src/scala/com/github/johnynek/bazel_deps:projectmodel `pwd`/3rdparty/workspace.bzl `pwd` /3rdparty/jvm
```

Then you should add 
```
load("//3rdparty:workspace.bzl", "maven_dependencies")
load("//3rdparty:load.bzl", "declare_maven")

maven_dependencies(declare_maven)
```

To your workspace to load the maven dependencies.

## Code
This code was originally forked from [pgr0ss/bazel-deps](https://github.com/pgr0ss/bazel-deps)

This code was inspired by the [aether examples](https://github.com/eclipse/aether-demo/blob/322fa556494335faaf3ad3b7dbe8f89aaaf6222d/aether-demo-snippets/src/main/java/org/eclipse/aether/examples/GetDependencyTree.java) for walking maven dependencies.
