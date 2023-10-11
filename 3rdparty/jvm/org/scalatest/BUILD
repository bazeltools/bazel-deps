load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
scala_import(
    name = "scalatest",
    exports = [
        "//3rdparty/jvm/org/scalactic:scalactic"
    ],
    jars = [
        "//external:jar/org/scalatest/scalatest_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_lang:scala_reflect",
        ":scalatest_core",
        ":scalatest_diagrams",
        ":scalatest_featurespec",
        ":scalatest_flatspec",
        ":scalatest_freespec",
        ":scalatest_funspec",
        ":scalatest_funsuite",
        ":scalatest_matchers_core",
        ":scalatest_mustmatchers",
        ":scalatest_propspec",
        ":scalatest_refspec",
        ":scalatest_shouldmatchers",
        ":scalatest_wordspec"
    ],
    visibility = [
        "//visibility:public"
    ]
)



java_library(
    name = "scalatest_compatible",
    exports = [
        "//external:jar/org/scalatest/scalatest_compatible"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "scalatest_core",
    jars = [
        "//external:jar/org/scalatest/scalatest_core_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang/modules:scala_xml",
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_lang:scala_reflect",
        "//3rdparty/jvm/org/scalactic:scalactic",
        ":scalatest_compatible"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "scalatest_diagrams",
    jars = [
        "//external:jar/org/scalatest/scalatest_diagrams_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_lang:scala_reflect",
        ":scalatest_core"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "scalatest_featurespec",
    jars = [
        "//external:jar/org/scalatest/scalatest_featurespec_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_lang:scala_reflect",
        ":scalatest_core"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "scalatest_flatspec",
    jars = [
        "//external:jar/org/scalatest/scalatest_flatspec_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_lang:scala_reflect",
        ":scalatest_core"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "scalatest_freespec",
    jars = [
        "//external:jar/org/scalatest/scalatest_freespec_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_lang:scala_reflect",
        ":scalatest_core"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "scalatest_funspec",
    jars = [
        "//external:jar/org/scalatest/scalatest_funspec_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_lang:scala_reflect",
        ":scalatest_core"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "scalatest_funsuite",
    jars = [
        "//external:jar/org/scalatest/scalatest_funsuite_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_lang:scala_reflect",
        ":scalatest_core"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "scalatest_matchers_core",
    jars = [
        "//external:jar/org/scalatest/scalatest_matchers_core_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_lang:scala_reflect",
        ":scalatest_core"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "scalatest_mustmatchers",
    jars = [
        "//external:jar/org/scalatest/scalatest_mustmatchers_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_lang:scala_reflect",
        ":scalatest_matchers_core"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "scalatest_propspec",
    jars = [
        "//external:jar/org/scalatest/scalatest_propspec_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_lang:scala_reflect",
        ":scalatest_core"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "scalatest_refspec",
    jars = [
        "//external:jar/org/scalatest/scalatest_refspec_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_lang:scala_reflect",
        ":scalatest_core"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "scalatest_shouldmatchers",
    jars = [
        "//external:jar/org/scalatest/scalatest_shouldmatchers_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_lang:scala_reflect",
        ":scalatest_matchers_core"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "scalatest_wordspec",
    jars = [
        "//external:jar/org/scalatest/scalatest_wordspec_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/scala_lang:scala_reflect",
        ":scalatest_core"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)


