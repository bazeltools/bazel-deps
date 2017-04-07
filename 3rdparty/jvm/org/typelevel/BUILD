scala_library(
    name = "cats_core",
    exports = [
        "//external:jar/org/typelevel/cats_core_2_11",
        ":cats_kernel"
    ],
    runtime_deps = [
        "//3rdparty/jvm/com/github/mpilquist:simulacrum",
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":cats_macros",
        ":machinist"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_library(
    name = "cats_free",
    exports = [
        "//external:jar/org/typelevel/cats_free_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/com/github/mpilquist:simulacrum",
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":cats_core",
        ":cats_macros",
        ":machinist"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_library(
    name = "cats_kernel",
    exports = [
        "//external:jar/org/typelevel/cats_kernel_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_library(
    name = "cats_macros",
    exports = [
        "//external:jar/org/typelevel/cats_macros_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/com/github/mpilquist:simulacrum",
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":machinist"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_library(
    name = "machinist",
    exports = [
        "//external:jar/org/typelevel/machinist_2_11"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_reflect"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_library(
    name = "macro_compat",
    exports = [
        "//external:jar/org/typelevel/macro_compat_2_11"
    ],
    visibility = [
        "//visibility:public"
    ]
)


