java_library(name = "aether_api",
             exports = ["@org_eclipse_aether_aether_api//jar"],
             visibility = ["//visibility:public"])

java_library(name = "aether_connector_basic",
             exports = [":aether_api",
                        ":aether_spi",
                        ":aether_util",
                        "@org_eclipse_aether_aether_connector_basic//jar"],
             visibility = ["//visibility:public"])

java_library(name = "aether_impl",
             exports = [":aether_api",
                        ":aether_spi",
                        ":aether_util",
                        "@org_eclipse_aether_aether_impl//jar"],
             visibility = ["//visibility:public"])

java_library(name = "aether_spi",
             exports = ["@org_eclipse_aether_aether_spi//jar"],
             visibility = ["//visibility:public"])

java_library(name = "aether_transport_file",
             exports = [":aether_api",
                        ":aether_spi",
                        ":aether_util",
                        "@org_eclipse_aether_aether_transport_file//jar"],
             visibility = ["//visibility:public"])

java_library(name = "aether_transport_http",
             exports = ["//3rdparty/jvm/org/apache/httpcomponents:httpclient",
                        "//3rdparty/jvm/org/slf4j:jcl_over_slf4j",
                        ":aether_api",
                        ":aether_spi",
                        ":aether_util",
                        "@org_eclipse_aether_aether_transport_http//jar"],
             visibility = ["//visibility:public"])

java_library(name = "aether_util",
             exports = ["@org_eclipse_aether_aether_util//jar"],
             visibility = ["//visibility:public"])
