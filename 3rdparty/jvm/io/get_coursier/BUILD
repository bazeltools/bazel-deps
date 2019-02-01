load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
scala_import(
    name = "coursier_cache",
    jars = [
        "//external:jar/io/get_coursier/coursier_cache_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":coursier_core"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "coursier_core",
    jars = [
        "//external:jar/io/get_coursier/coursier_core_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang/modules:scala_xml",
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)


