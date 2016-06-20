scala_library(name = "circe_core",
    exports = ["//3rdparty/jvm/org/scala_lang:scala_library",
               "//3rdparty/jvm/org/typelevel:cats_core",
               ":circe_numbers_2_11",
               "@io_circe_circe_core_2_11//jar:file"],
    visibility = ["//visibility:public"])

scala_library(name = "circe_generic",
    exports = ["//3rdparty/jvm/com/chuusai:shapeless_2_11",
               "//3rdparty/jvm/org/scala_lang:scala_library",
               "//3rdparty/jvm/org/typelevel:macro_compat_2_11",
               ":circe_core",
               "@io_circe_circe_generic_2_11//jar:file"],
    visibility = ["//visibility:public"])

scala_library(name = "circe_jackson",
    exports = ["//3rdparty/jvm/com/fasterxml/jackson/core:jackson_core",
               "//3rdparty/jvm/com/fasterxml/jackson/core:jackson_databind",
               "//3rdparty/jvm/org/scala_lang:scala_library",
               ":circe_core",
               "@io_circe_circe_jackson_2_11//jar:file"],
    visibility = ["//visibility:public"])

java_library(name = "circe_jawn_2_11",
    exports = ["//3rdparty/jvm/org/spire_math:jawn_parser_2_11",
               "@io_circe_circe_jawn_2_11//jar"],
    visibility = ["//visibility:public"])

java_library(name = "circe_numbers_2_11",
    exports = ["@io_circe_circe_numbers_2_11//jar"],
    visibility = ["//visibility:public"])

scala_library(name = "circe_parser",
    exports = ["//3rdparty/jvm/org/scala_lang:scala_library",
               ":circe_core",
               ":circe_jawn_2_11",
               "@io_circe_circe_parser_2_11//jar:file"],
    visibility = ["//visibility:public"])