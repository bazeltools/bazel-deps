java_library(
    name = "maven_aether_provider",
    exports = [
        "//external:jar/org/apache/maven/maven_aether_provider"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/codehaus/plexus:plexus_component_annotations",
        "//3rdparty/jvm/org/codehaus/plexus:plexus_utils",
        "//3rdparty/jvm/org/eclipse/aether:aether_api",
        "//3rdparty/jvm/org/eclipse/aether:aether_impl",
        "//3rdparty/jvm/org/eclipse/aether:aether_spi",
        "//3rdparty/jvm/org/eclipse/aether:aether_util",
        "//3rdparty/jvm/org/eclipse/sisu:org_eclipse_sisu_plexus",
        ":maven_model",
        ":maven_model_builder",
        ":maven_repository_metadata"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "maven_model",
    exports = [
        "//external:jar/org/apache/maven/maven_model"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "maven_model_builder",
    exports = [
        "//external:jar/org/apache/maven/maven_model_builder"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/codehaus/plexus:plexus_interpolation"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "maven_repository_metadata",
    exports = [
        "//external:jar/org/apache/maven/maven_repository_metadata"
    ],
    visibility = [
        "//visibility:public"
    ]
)


