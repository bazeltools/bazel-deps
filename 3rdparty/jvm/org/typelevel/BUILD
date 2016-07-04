scala_library(name = "cats_core",
              exports = ["//3rdparty/jvm/com/github/mpilquist:simulacrum",
                         "//3rdparty/jvm/org/scala_lang:scala_library",
                         "//external:jar/org/typelevel/cats_core_2_11",
                         ":cats_kernel",
                         ":cats_macros",
                         ":machinist"],
              visibility = ["//visibility:public"])

scala_library(name = "cats_free",
              exports = ["//3rdparty/jvm/com/github/mpilquist:simulacrum",
                         "//3rdparty/jvm/org/scala_lang:scala_library",
                         "//external:jar/org/typelevel/cats_free_2_11",
                         ":cats_core",
                         ":cats_macros",
                         ":machinist"],
              visibility = ["//visibility:public"])

scala_library(name = "cats_kernel",
              exports = ["//3rdparty/jvm/org/scala_lang:scala_library",
                         "//external:jar/org/typelevel/cats_kernel_2_11"],
              visibility = ["//visibility:public"])

scala_library(name = "cats_macros",
              exports = ["//3rdparty/jvm/com/github/mpilquist:simulacrum",
                         "//3rdparty/jvm/org/scala_lang:scala_library",
                         "//external:jar/org/typelevel/cats_macros_2_11",
                         ":machinist"],
              visibility = ["//visibility:public"])

scala_library(name = "machinist",
              exports = ["//3rdparty/jvm/org/scala_lang:scala_reflect",
                         "//external:jar/org/typelevel/machinist_2_11"],
              visibility = ["//visibility:public"])

scala_library(name = "macro_compat",
              exports = ["//external:jar/org/typelevel/macro_compat_2_11"],
              visibility = ["//visibility:public"])
