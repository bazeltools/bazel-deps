# bazel-deps

Generate [bazel](http://bazel.io/) dependencies transitively for maven artifacts, with scala
support.

## Usage

Run parseproject on your project yaml file. For instance, this project is setup with:

```bash
bazel run //:parse -- generate -r `pwd` -s 3rdparty/workspace.bzl -d dependencies.yaml
```

We give three arguments: the path to the file we will include in our workspace. The path to the root
of our bazel repo. The path to the dependencies file. You can also run with `--help`.

This will create a tree of BUILD files that match the maven group id, and the artifact id will be
a label in a BUILD file. You should not edit these by hand, and instead have a separate directory
for any exceptions that you manage along with [Replacements](#replacements).

Then you should add
```
load("//3rdparty:workspace.bzl", "maven_dependencies")

maven_dependencies()
```
to your workspace to load the maven dependencies.

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
and [`options`](#options). Important: only dependencies explicitly named have public visibility,
transitive dependencies not listed in the dependencies file have visibility limited to the third
party directory.

### <a name="dependencies">Dependencies</a>

Dependencies are a map from maven group id to artifact id, with some metadata, such as:
```yaml
dependencies:
  com.google.guava:
    guava:
      version: "18.0"
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
The `version` field is optional. If it is absent, it means this jar is expected to be found by
transitive dependencies, and it is available to be used outside of the thirdparty directory, but the
exact version used can be selected according to the version resolution rules. It is an error to have
an unversioned dependency that is not a transitive dependency of another versioned dependency.

A target may optionally add `exports` and `exclude` lists to a dependency. `exports` should be just the group and
artifact (such as: `com.twitter:scalding-core` in the above), and they should be listed in the dependencies. `exclude`
list should also be only the group and artifact.

Each group id can only appear once, so you should collocate dependencies by group. WARNING the parsing library
we are using does not fail on duplicate keys, it just takes the last one, so watch out. It would be good
to fix that, but writing a new yaml parser is out of scope.

#### <a name="packaging-classifiers">Packaging and Classifiers</a>

Depending on artifacts with classifiers is straightforward: just add the packaging and classifier as part of the
artifact id:

```yaml
dependencies:
  net.sf.json-lib:
    json-lib:jar:jdk15: # artifact:packaging:classifier
      lang: java
      version: "2.4"
```

**Note**: Currently, only `jar` packaging is supported for dependencies. More work is needed on the `bazel-deps` backend
to ensure that non-jar dependencies are written as `data` attributes, instead of regular jar dependencies. 

Excluding artifacts with packaging or classifiers is similar to including dependencies. Non-jar packaging _is_ supported
for `exclude`.

```yaml
  com.amazonaws:
    DynamoDBLocal:
      lang: java
      version: "1.11.86"
      exclude:
        - "com.almworks.sqlite4java:sqlite4java-win32-x86:dll"
        - "com.almworks.sqlite4java:sqlite4java-win32-x64:dll"
        - "com.almworks.sqlite4java:libsqlite4java-osx:dylib"
        - "com.almworks.sqlite4java:libsqlite4java-linux-i386:so"
        - "com.almworks.sqlite4java:libsqlite4java-linux-amd64:so"
```

#### <a name="annotation-processors">Annotation Processors (`processorClasses`)</a>

A target may also optionally add `processorClasses` to a dependency. This is for [annotation processors](https://docs.oracle.com/javase/8/docs/api/javax/annotation/processing/Processor.html).
`bazel-deps` will generate a `java_library` and a `java_plugin` for each annotation processor defined. For example, we can define Google's auto-value annotation processor via:
```
dependencies:
  com.google.auto.value:
    auto-value:
      version: "1.5"
      lang: java
      processorClasses: ["com.google.auto.value.processor.AutoValueProcessor"]
```
This will yield the following:
```
java_library(
    name = "auto_value",
    exported_plugins = [
        ":auto_value_plugin",
    ],
    visibility = [
        "//visibility:public",
    ],
    exports = [
        "//external:jar/com/google/auto/value/auto_value",
    ],
)

java_plugin(
    name = "auto_value_plugin",
    processor_class = "com.google.auto.value.processor.AutoValueProcessor",
    deps = [
        "//external:jar/com/google/auto/value/auto_value",
    ],
)
``` 
If there is only a single `processorClasses` defined, the `java_plugin` rule is named `<java_library_name>_plugin`. If there are multiple
`processorClasses` defined, each one is named `<java_library_name>_plugin_<processor_class_to_snake_case>`.

### <a name="options">Options</a>
In the options we set:

* buildHeader: usually you will want to configure your scala support here:
```
  buildHeader:
    - load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
```
* languages: java and scala
* thirdPartyDirectory: path to where we write the BUILD files for thirdparty. The default is `3rdparty/jvm`.
* versionConflictPolicy: `fixed`, `fail` or `highest`
* transitivity: `runtime_deps` or `exports`
* resolvers: the maven servers to use.
* resolverCache: where bazel-deps should cache resolved packages.  `local` (`target/local-repo` in the repository root)
  or `bazel_output_base` (`bazel-deps/local-repo` inside the repository's Bazel output base -- from `bazel info
  output_base`)
* namePrefix: a string added to the generated workspace names, to avoid conflicts.  The external repository names and
  binding targets of each dependency are prefixed.
* strictVisibility: this is enabled by default, when enabled a target must be explicitly declared in the 
  `dependencies.yaml` file or it will not be visible to the rest of the workspace. If it is set to `false` all targets 
  will be generated with `public` visibility.
* licenses: a set of strings added a licenses rule to each generated bazel target.  Required by
  bazel if your build targets are under third_party/
* resolverType: the string aether or coursier. `aether` is the default, but it is slower and seems
  to silently miss some dependencies for reasons we don't yet understand. Coursier will likely be
  the default in the future, but for now it is opt in.

In the default case, with no options given, we use:
- `highest` versionConflictPolicy
- `exports` transitivity
- allow java and scala `2.11`
- use maven central as the resolver
- `local` resolverCache
- empty namePrefix (`""`)

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
      target: "@io_bazel_rules_scala_scala_library"
    scala-reflect:
      lang: scala/unmangled
      target: "@io_bazel_rules_scala_scala_reflect"
```

In this way, we redirect maven deps to those providers.

Note, we stop walking the graph when we see a replaced node, so the replacement target is now
responsible for building correctly, and correctly exporting any dependencies that need to be
on the compile classpath.

## Code
This code was originally forked from [pgr0ss/bazel-deps](https://github.com/pgr0ss/bazel-deps)

This code was inspired by the [aether examples](https://github.com/eclipse/aether-demo/blob/322fa556494335faaf3ad3b7dbe8f89aaaf6222d/aether-demo-snippets/src/main/java/org/eclipse/aether/examples/GetDependencyTree.java) for walking maven dependencies.
