java_library(name = "maven_aether_provider",
             exports = ["//3rdparty/jvm/org/codehaus/plexus:plexus_component_annotations",
                        "//3rdparty/jvm/org/codehaus/plexus:plexus_utils",
                        "//3rdparty/jvm/org/eclipse/aether:aether_api",
                        "//3rdparty/jvm/org/eclipse/aether:aether_impl",
                        "//3rdparty/jvm/org/eclipse/aether:aether_spi",
                        "//3rdparty/jvm/org/eclipse/aether:aether_util",
                        "//3rdparty/jvm/org/eclipse/sisu:org_eclipse_sisu_plexus",
                        ":maven_model",
                        ":maven_model_builder",
                        ":maven_repository_metadata",
                        "@org_apache_maven_maven_aether_provider//jar"],
             visibility = ["//visibility:public"])

java_library(name = "maven_model",
             exports = ["@org_apache_maven_maven_model//jar"],
             visibility = ["//visibility:public"])

java_library(name = "maven_model_builder",
             exports = ["//3rdparty/jvm/org/codehaus/plexus:plexus_interpolation",
                        "@org_apache_maven_maven_model_builder//jar"],
             visibility = ["//visibility:public"])

java_library(name = "maven_repository_metadata",
             exports = ["@org_apache_maven_maven_repository_metadata//jar"],
             visibility = ["//visibility:public"])
