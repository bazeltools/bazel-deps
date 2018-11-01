load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
java_library(
    name = "maven_aether_provider",
    exports = [
        "//external:jar/org/apache/maven/maven_aether_provider"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/apache/commons:commons_lang3",
        "//3rdparty/jvm/org/codehaus/plexus:plexus_component_annotations",
        "//3rdparty/jvm/org/codehaus/plexus:plexus_utils",
        "//3rdparty/jvm/org/eclipse/aether:aether_api",
        "//3rdparty/jvm/org/eclipse/aether:aether_impl",
        "//3rdparty/jvm/org/eclipse/aether:aether_spi",
        "//3rdparty/jvm/org/eclipse/aether:aether_util",
        ":maven_model",
        ":maven_model_builder",
        ":maven_repository_metadata"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "maven_artifact",
    exports = [
        "//external:jar/org/apache/maven/maven_artifact"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/apache/commons:commons_lang3",
        "//3rdparty/jvm/org/codehaus/plexus:plexus_utils"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



java_library(
    name = "maven_builder_support",
    exports = [
        "//external:jar/org/apache/maven/maven_builder_support"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/apache/commons:commons_lang3",
        "//3rdparty/jvm/org/codehaus/plexus:plexus_utils"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



java_library(
    name = "maven_model",
    exports = [
        "//external:jar/org/apache/maven/maven_model"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/apache/commons:commons_lang3",
        "//3rdparty/jvm/org/codehaus/plexus:plexus_utils"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



java_library(
    name = "maven_model_builder",
    exports = [
        "//external:jar/org/apache/maven/maven_model_builder"
    ],
    runtime_deps = [
        "//3rdparty/jvm/com/google/guava:guava",
        "//3rdparty/jvm/org/apache/commons:commons_lang3",
        "//3rdparty/jvm/org/codehaus/plexus:plexus_component_annotations",
        "//3rdparty/jvm/org/codehaus/plexus:plexus_interpolation",
        "//3rdparty/jvm/org/codehaus/plexus:plexus_utils",
        ":maven_artifact",
        ":maven_builder_support",
        ":maven_model"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



java_library(
    name = "maven_repository_metadata",
    exports = [
        "//external:jar/org/apache/maven/maven_repository_metadata"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/codehaus/plexus:plexus_utils"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



java_library(
    name = "maven_settings",
    exports = [
        "//external:jar/org/apache/maven/maven_settings"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/codehaus/plexus:plexus_utils"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "maven_settings_builder",
    exports = [
        "//external:jar/org/apache/maven/maven_settings_builder"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/apache/commons:commons_lang3",
        "//3rdparty/jvm/org/codehaus/plexus:plexus_component_annotations",
        "//3rdparty/jvm/org/codehaus/plexus:plexus_interpolation",
        "//3rdparty/jvm/org/codehaus/plexus:plexus_utils",
        "//3rdparty/jvm/org/sonatype/plexus:plexus_sec_dispatcher",
        ":maven_builder_support",
        ":maven_settings"
    ],
    visibility = [
        "//visibility:public"
    ]
)


