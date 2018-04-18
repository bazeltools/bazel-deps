load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
java_library(
    name = "aether_api",
    exports = [
        "//external:jar/org/eclipse/aether/aether_api"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "aether_connector_basic",
    exports = [
        "//external:jar/org/eclipse/aether/aether_connector_basic"
    ],
    runtime_deps = [
        ":aether_api",
        ":aether_spi",
        ":aether_util"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "aether_impl",
    exports = [
        "//external:jar/org/eclipse/aether/aether_impl"
    ],
    runtime_deps = [
        ":aether_api",
        ":aether_spi",
        ":aether_util"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "aether_spi",
    exports = [
        "//external:jar/org/eclipse/aether/aether_spi"
    ],
    runtime_deps = [
        ":aether_api"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "aether_transport_file",
    exports = [
        "//external:jar/org/eclipse/aether/aether_transport_file"
    ],
    runtime_deps = [
        ":aether_api",
        ":aether_spi",
        ":aether_util"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "aether_transport_http",
    exports = [
        "//external:jar/org/eclipse/aether/aether_transport_http"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/apache/httpcomponents:httpclient",
        "//3rdparty/jvm/org/slf4j:jcl_over_slf4j",
        ":aether_api",
        ":aether_spi",
        ":aether_util"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "aether_util",
    exports = [
        "//external:jar/org/eclipse/aether/aether_util"
    ],
    runtime_deps = [
        ":aether_api"
    ],
    visibility = [
        "//visibility:public"
    ]
)


