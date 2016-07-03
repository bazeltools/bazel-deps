scala_library(name = "cats_core",
              exports = ["//3rdparty/jvm/com/github/mpilquist:simulacrum",
                         "//3rdparty/jvm/org/scala_lang:scala_library",
                         ":cats_kernel",
                         ":cats_macros",
                         ":machinist",
                         "@org_typelevel_cats_core_2_11//jar:file"],
              visibility = ["//visibility:public"])

scala_library(name = "cats_free",
              exports = ["//3rdparty/jvm/com/github/mpilquist:simulacrum",
                         "//3rdparty/jvm/org/scala_lang:scala_library",
                         ":cats_core",
                         ":cats_macros",
                         ":machinist",
                         "@org_typelevel_cats_free_2_11//jar:file"],
              visibility = ["//visibility:public"])

scala_library(name = "cats_kernel",
              exports = ["//3rdparty/jvm/org/scala_lang:scala_library",
                         "@org_typelevel_cats_kernel_2_11//jar:file"],
              visibility = ["//visibility:public"])

scala_library(name = "cats_macros",
              exports = ["//3rdparty/jvm/com/github/mpilquist:simulacrum",
                         "//3rdparty/jvm/org/scala_lang:scala_library",
                         ":machinist",
                         "@org_typelevel_cats_macros_2_11//jar:file"],
              visibility = ["//visibility:public"])

scala_library(name = "machinist",
              exports = ["//3rdparty/jvm/org/scala_lang:scala_reflect",
                         "@org_typelevel_machinist_2_11//jar:file"],
              visibility = ["//visibility:public"])

scala_library(name = "macro_compat",
              exports = ["//3rdparty/jvm/org/scala_lang:scala_library",
                         "@org_typelevel_macro_compat_2_11//jar:file"],
              visibility = ["//visibility:public"])
