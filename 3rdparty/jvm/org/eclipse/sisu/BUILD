load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
java_library(
    name = "org_eclipse_sisu_inject",
    exports = [
        "//external:jar/org/eclipse/sisu/org_eclipse_sisu_inject"
    ],
    runtime_deps = [
        "//3rdparty/jvm/asm:asm"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "org_eclipse_sisu_plexus",
    exports = [
        "//external:jar/org/eclipse/sisu/org_eclipse_sisu_plexus"
    ],
    runtime_deps = [
        "//3rdparty/jvm/com/google/guava:guava",
        "//3rdparty/jvm/javax/enterprise:cdi_api",
        "//3rdparty/jvm/org/codehaus/plexus:plexus_classworlds",
        ":org_eclipse_sisu_inject"
    ],
    visibility = [
        "//visibility:public"
    ]
)


