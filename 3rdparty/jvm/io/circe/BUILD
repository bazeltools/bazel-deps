load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
scala_import(
    name = "circe_core",
    jars = [
        "//external:jar/io/circe/circe_core_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/typelevel:cats_core",
        ":circe_numbers"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "circe_generic",
    exports = [
        "//3rdparty/jvm/com/chuusai:shapeless",
        "//3rdparty/jvm/org/typelevel:cats_core",
        "//3rdparty/jvm/org/typelevel:cats_kernel",
        "//3rdparty/jvm/org/typelevel:macro_compat"
    ],
    jars = [
        "//external:jar/io/circe/circe_generic_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":circe_core"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "circe_jackson210",
    jars = [
        "//external:jar/io/circe/circe_jackson210_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/com/fasterxml/jackson/core:jackson_core",
        "//3rdparty/jvm/com/fasterxml/jackson/core:jackson_databind",
        "//3rdparty/jvm/org/scala_lang:scala_library",
        ":circe_core"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "circe_jawn",
    jars = [
        "//external:jar/io/circe/circe_jawn_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library",
        "//3rdparty/jvm/org/typelevel:jawn_parser",
        ":circe_core"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "circe_numbers",
    jars = [
        "//external:jar/io/circe/circe_numbers_2_12"
    ],
    runtime_deps = [
        "//3rdparty/jvm/org/scala_lang:scala_library"
    ],
    visibility = [
        "//3rdparty/jvm:__subpackages__"
    ]
)


