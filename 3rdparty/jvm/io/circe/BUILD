scala_library(name = "circe_core",
              exports = ["//3rdparty/jvm/org/scala_lang:scala_library",
                         "//3rdparty/jvm/org/typelevel:cats_core",
                         "//external:jar/io/circe/circe_core_2_11",
                         ":circe_numbers_2_11"],
              visibility = ["//visibility:public"])

scala_library(name = "circe_generic",
              exports = ["//3rdparty/jvm/com/chuusai:shapeless",
                         "//3rdparty/jvm/org/scala_lang:scala_library",
                         "//3rdparty/jvm/org/typelevel:macro_compat",
                         "//external:jar/io/circe/circe_generic_2_11",
                         ":circe_core"],
              visibility = ["//visibility:public"])

scala_library(name = "circe_jackson",
              exports = ["//3rdparty/jvm/com/fasterxml/jackson/core:jackson_core",
                         "//3rdparty/jvm/com/fasterxml/jackson/core:jackson_databind",
                         "//3rdparty/jvm/org/scala_lang:scala_library",
                         "//external:jar/io/circe/circe_jackson_2_11",
                         ":circe_core"],
              visibility = ["//visibility:public"])

java_library(name = "circe_jawn_2_11",
             exports = ["//3rdparty/jvm/org/spire_math:jawn_parser_2_11",
                        "//external:jar/io/circe/circe_jawn_2_11"],
             visibility = ["//visibility:public"])

java_library(name = "circe_numbers_2_11",
             exports = ["//external:jar/io/circe/circe_numbers_2_11"],
             visibility = ["//visibility:public"])

scala_library(name = "circe_parser",
              exports = ["//3rdparty/jvm/org/scala_lang:scala_library",
                         "//external:jar/io/circe/circe_parser_2_11",
                         ":circe_core",
                         ":circe_jawn_2_11"],
              visibility = ["//visibility:public"])
