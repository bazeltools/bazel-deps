# bazel-deps

Generate [bazel](http://bazel.io/) dependencies transitively for maven artifacts, with scala
support.

## Usage

Run parseproject on your project yaml file. For instance, this project is setup with:

```bash
bazel build src/scala/com/github/johnynek/bazel_deps:parseproject_deploy.jar
./gen_maven_deps.sh `pwd` 3rdparty/workspace.bzl dependencies.yaml
```
We give three arguments: the path to the file we will include in our workspace. The path to the root
of our bazel repo. The path to the dependencies file.

This will create a tree of BUILD files that match the maven group id, and the artifact id will be
a label in a BUILD file. You should not edit these by hand, and instead have a separate directory
for any exceptions that you manage along with [Replacements](#replacements).

Then you should add
```
load("//3rdparty:workspace.bzl", "maven_dependencies")
load("//3rdparty:load.bzl", "declare_maven")

maven_dependencies(declare_maven)
```

To your workspace to load the maven dependencies. Note you will need to implement load.bzl `declare_maven`. A standard implementation
might be:
```python
def declare_maven(hash):
    native.maven_jar(
        name = hash["name"],
        artifact = hash["artifact"],
        sha1 = hash["sha1"]
    )
    native.bind(name = hash["bind"],
                actual = hash["actual"]
                )
```

## Assumptions and usage
This tool will generate one canonical version for every jar in the transitive dependencies of
the root dependencies declared. You have three conflict resolution modes currently (which currently
apply globally):

- fail: if more than one version is found transitively, fail.
- fixed: for all artifacts explicitly added, use that version, otherwise fail if any other artifact has multiple versions.
- highest: for all artifacts explicitly added, use that version, otherwise take the highest version.

In any case, we add a comment for any duplicates found in the workspace loading file.

To declare dependencies, add items to the `dependencies` key in your declaration file. The format
should be yaml or json. It should have [`dependencies`](#dependencies) and it may have [`replacements`](#replacements)
and [`options`](#options).

### <a name="dependencies">Dependencies</a>

Dependencies are a map from maven group id to artifact id, with some metadata, such as:
```yaml
dependencies:
  com.google.guava:
    guava:
      version: 18.0
      lang: java
```
Language is always required and may be one of `java, scala, scala/unmangled`. To control the scala
version, see the [Options section](#options). A common case are projects with many modules. For instance in
the [scalding project](https://github.com/twitter/scalding) there are many modules: `-core, -date,
-args, -db, -avro` to name a few. To reduce duplication you can do:

```
dependencies:
  com.twitter:
    scalding:
      version: 0.16.0
      lang: scala
      modules: [core, date, args, db, arvo]
```

Each group id can only appear once (but the parser unfortunately does
not fail if you fail to ensure this), so you should collocate dependencies by group.

### <a name="options">Options</a>
In the options we set:

* languages: java and scala
* versionConflictPolicy: `fixed`, `fail` or `highest`
* transitivity: `runtime_deps` or `exports`
* resolvers: the maven servers to use.

In the default case, with no options given, we use the `highest` versionConflictPolicy,
exports transitivity, allow java and scala `2.11`, and use maven central as the resolver.

### <a name="replacements">Replacements</a>
Some maven jars should not be used and instead are replaced by internal targets. Here are
some examples of this:

1. A subproject in the repo is published as a maven artifact (`A`). Others (`B`) depend on this artifact (`B -> A`) and in turn we depend on those (we have added `B` to our dependencies file). We don't want to pull `A` from a maven repo, since we build it internally, so we replace that artifact with an internal target.
2. We get some scala artifacts directly from the sdk. So, if a jar says it needs `org.scala-lang:scala-library` we already have that (and a few other jars) declared, and we don't want to risk having two potentially incompatible versions.
3. A small external project has both a bazel build and a maven publishing. We prefer to use the bazel build so we can easily pull more recent versions by bumping up a gitsha rather than waiting for jar to be published.

The replacements work on the level of artifacts. An artifact is replaced one-for-one with a local
bazel target. For instance:
```yaml
replacements:
  org.scala-lang:
    scala-library:
      lang: scala/unmangled # scala-library is not mangled like sbt does with other jars
      target: "//3rdparty/manual:scala_library_file"
    scala-reflect:
      lang: scala/unmangled
      target: "//3rdparty/manual:scala_reflect_file"
```

Where we have added:
```python
filegroup(name = "scala_reflect_file",
          srcs = ["@scala//:lib/scala-reflect.jar"],
          visibility = ["//visibility:public"])

filegroup(name = "scala_library_file",
          srcs = ["@scala//:lib/scala-library.jar"],
          visibility = ["//visibility:public"])
```
to the `3rdparty/manual/BUILD` file. In this way, we redirect maven deps to those providers.

Note, we stop walking the graph when we see a replaced node, so the replacement target is now
responsible for building correctly, and correctly exporting any dependencies that need to be
on the compile classpath.

## Code
This code was originally forked from [pgr0ss/bazel-deps](https://github.com/pgr0ss/bazel-deps)

This code was inspired by the [aether examples](https://github.com/eclipse/aether-demo/blob/322fa556494335faaf3ad3b7dbe8f89aaaf6222d/aether-demo-snippets/src/main/java/org/eclipse/aether/examples/GetDependencyTree.java) for walking maven dependencies.
