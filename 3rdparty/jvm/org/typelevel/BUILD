load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
scala_import(
    name = "cats_core",
    exports = [
        ":cats_kernel"
    ],
    jars = [
        "//external:jar/org/typelevel/cats_core_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":cats_macros"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "cats_effect",
    jars = [
        "//external:jar/org/typelevel/cats_effect_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":cats_core"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "cats_free",
    jars = [
        "//external:jar/org/typelevel/cats_free_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":cats_core",
        ":cats_macros"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "cats_kernel",
    jars = [
        "//external:jar/org/typelevel/cats_kernel_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "cats_macros",
    jars = [
        "//external:jar/org/typelevel/cats_macros_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "jawn_parser",
    jars = [
        "//external:jar/org/typelevel/jawn_parser_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)



scala_import(
    name = "macro_compat",
    jars = [
        "//external:jar/org/typelevel/macro_compat_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "paiges_core",
    jars = [
        "//external:jar/org/typelevel/paiges_core_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)


