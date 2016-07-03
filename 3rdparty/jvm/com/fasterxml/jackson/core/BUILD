java_library(name = "jackson_annotations",
             exports = ["@com_fasterxml_jackson_core_jackson_annotations//jar"],
             visibility = ["//visibility:public"])

java_library(name = "jackson_core",
             exports = ["@com_fasterxml_jackson_core_jackson_core//jar"],
             visibility = ["//visibility:public"])

java_library(name = "jackson_databind",
             exports = [":jackson_annotations",
                        "@com_fasterxml_jackson_core_jackson_databind//jar"],
             visibility = ["//visibility:public"])
