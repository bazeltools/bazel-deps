java_library(name = "org_eclipse_sisu_inject",
             exports = ["//3rdparty/jvm/asm:asm",
                        "//external:jar/org/eclipse/sisu/org_eclipse_sisu_inject"],
             visibility = ["//visibility:public"])

java_library(name = "org_eclipse_sisu_plexus",
             exports = ["//3rdparty/jvm/com/google/guava:guava",
                        "//3rdparty/jvm/javax/enterprise:cdi_api",
                        "//3rdparty/jvm/org/codehaus/plexus:plexus_classworlds",
                        "//external:jar/org/eclipse/sisu/org_eclipse_sisu_plexus",
                        ":org_eclipse_sisu_inject"],
             visibility = ["//visibility:public"])
