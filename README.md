# bazel-deps

Generate bazel dependencies for maven artifacts

## Usage

```bash
mvn package
java -jar target/bazel-deps-1.0-SNAPSHOT.jar <maven artifact>...
```

## Example

```bash
% java -jar target/bazel-deps-1.0-SNAPSHOT.jar com.fasterxml.jackson.core:jackson-databind:2.5.0 junit:junit:jar:4.12


--------- Add these lines to your WORKSPACE file ---------

maven_jar(name = "org/hamcrest/hamcrest-core", artifact = "org.hamcrest:hamcrest-core:jar:1.3")
maven_jar(name = "com/fasterxml/jackson/core/jackson-annotations", artifact = "com.fasterxml.jackson.core:jackson-annotations:jar:2.5.0")
maven_jar(name = "com/fasterxml/jackson/core/jackson-core", artifact = "com.fasterxml.jackson.core:jackson-core:jar:2.5.0")
maven_jar(name = "com/fasterxml/jackson/core/jackson-databind", artifact = "com.fasterxml.jackson.core:jackson-databind:jar:2.5.0")
maven_jar(name = "junit/junit", artifact = "junit:junit:jar:4.12")


--------- Add these lines to your BUILD file ---------

java_library(
  name="jackson-databind",
  visibility = ["//visibility:public"],
  exports = [
    "@com/fasterxml/jackson/core/jackson-annotations//jar",
    "@com/fasterxml/jackson/core/jackson-core//jar",
    "@com/fasterxml/jackson/core/jackson-databind//jar",
  ],
)

java_library(
  name="junit",
  visibility = ["//visibility:public"],
  exports = [
    "@junit/junit//jar",
    "@org/hamcrest/hamcrest-core//jar",
  ],
)
```

You can also exclude a set of transitive dependencies:

```bash

% java -jar target/bazel-deps-1.0-SNAPSHOT.jar -x io.dropwizard:dropwizard-core:0.8.1 io.dropwizard:dropwizard-client:0.8.1


--------- Add these lines to your WORKSPACE file ---------

maven_jar(name = "commons-codec/commons-codec", artifact = "commons-codec:commons-codec:jar:1.6")
maven_jar(name = "io/dropwizard/dropwizard-client", artifact = "io.dropwizard:dropwizard-client:jar:0.8.1")
maven_jar(name = "io/dropwizard/dropwizard-core", artifact = "io.dropwizard:dropwizard-core:jar:0.8.1")
maven_jar(name = "org/apache/httpcomponents/httpclient", artifact = "org.apache.httpcomponents:httpclient:jar:4.3.5")
maven_jar(name = "org/apache/httpcomponents/httpcore", artifact = "org.apache.httpcomponents:httpcore:jar:4.3.2")
maven_jar(name = "org/glassfish/jersey/connectors/jersey-apache-connector", artifact = "org.glassfish.jersey.connectors:jersey-apache-connector:jar:2.17")
maven_jar(name = "io/dropwizard/metrics/metrics-httpclient", artifact = "io.dropwizard.metrics:metrics-httpclient:jar:3.1.1")


--------- Add these lines to your BUILD file ---------

java_library(
  name="dropwizard-client",
  visibility = ["//visibility:public"],
  exports = [
    "@commons-codec/commons-codec//jar",
    "@io/dropwizard/dropwizard-client//jar",
    "@io/dropwizard/dropwizard-core//jar",
    "@io/dropwizard/metrics/metrics-httpclient//jar",
    "@org/apache/httpcomponents/httpclient//jar",
    "@org/apache/httpcomponents/httpcore//jar",
    "@org/glassfish/jersey/connectors/jersey-apache-connector//jar",
  ],
)
```

## Code

This code was inspired by the aether examples for walking maven dependencies: https://github.com/eclipse/aether-demo/blob/322fa556494335faaf3ad3b7dbe8f89aaaf6222d/aether-demo-snippets/src/main/java/org/eclipse/aether/examples/GetDependencyTree.java
