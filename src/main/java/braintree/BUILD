java_binary(name = "bazeldeps",
             srcs = ["BazelDeps.java"],
             main_class = "braintree.BazelDeps",
             deps = [":maven",
                  "//3rdparty:aether-api",
                  "//3rdparty:aether-connector-basic",
                  "//3rdparty:aether-impl",
                  "//3rdparty:aether-transport-file",
                  "//3rdparty:aether-transport-http",
                  "//3rdparty:args4j",
                  "//3rdparty:guava",
                  "//3rdparty:maven-aether-provider"]
             )

java_library(name = "maven",
             srcs = ["Maven.java"],
             deps = ["//3rdparty:aether-api",
                  "//3rdparty:aether-connector-basic",
                  "//3rdparty:aether-impl",
                  "//3rdparty:aether-transport-file",
                  "//3rdparty:aether-transport-http",
                  "//3rdparty:args4j",
                  "//3rdparty:guava",
                  "//3rdparty:maven-aether-provider"]
             )
