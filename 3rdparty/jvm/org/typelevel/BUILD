scala_library(name = "cats_core",
    exports = ["//3rdparty/jvm/com/github/mpilquist:simulacrum_2_11",
               ":cats_kernel_2_11",
               ":cats_macros_2_11",
               ":machinist",
               "@org_typelevel_cats_core_2_11//jar:file"],
    visibility = ["//visibility:public"])

java_library(name = "cats_kernel_2_11",
    exports = ["@org_typelevel_cats_kernel_2_11//jar"],
    visibility = ["//visibility:public"])

java_library(name = "cats_macros_2_11",
    exports = ["@org_typelevel_cats_macros_2_11//jar"],
    visibility = ["//visibility:public"])

scala_library(name = "machinist",
    exports = ["//3rdparty/jvm/org/scala_lang:scala_reflect",
               "@org_typelevel_machinist_2_11//jar:file"],
    visibility = ["//visibility:public"])

java_library(name = "macro_compat_2_11",
    exports = ["@org_typelevel_macro_compat_2_11//jar"],
    visibility = ["//visibility:public"])