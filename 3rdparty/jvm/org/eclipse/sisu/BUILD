java_library(name = "org_eclipse_sisu_inject",
    exports = ["//3rdparty/jvm/asm:asm",
               "@org_eclipse_sisu_org_eclipse_sisu_inject//jar"],
    visibility = ["//visibility:public"])

java_library(name = "org_eclipse_sisu_plexus",
    exports = ["//3rdparty/jvm/com/google/guava:guava",
               "//3rdparty/jvm/javax/enterprise:cdi_api",
               "//3rdparty/jvm/org/codehaus/plexus:plexus_classworlds",
               ":org_eclipse_sisu_inject",
               "@org_eclipse_sisu_org_eclipse_sisu_plexus//jar"],
    visibility = ["//visibility:public"])