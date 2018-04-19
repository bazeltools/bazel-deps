# Do not edit. bazel-deps autogenerates this file from dependencies.yaml.

def declare_maven(hash):
    native.maven_jar(
        name = hash["name"],
        artifact = hash["artifact"],
        sha1 = hash["sha1"],
        repository = hash["repository"]
    )
    native.bind(
        name = hash["bind"],
        actual = hash["actual"]
    )

def list_dependencies():
    return [
    {"artifact": "com.chuusai:shapeless_2.11:2.3.2", "lang": "scala", "sha1": "f40ed6e303d550293f5f8f3743681d98e31f2360", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_chuusai_shapeless_2_11", "actual": "@com_chuusai_shapeless_2_11//jar:file", "bind": "jar/com/chuusai/shapeless_2_11"},
    {"artifact": "com.fasterxml.jackson.core:jackson-annotations:2.5.0", "lang": "java", "sha1": "a2a55a3375bc1cef830ca426d68d2ea22961190e", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_fasterxml_jackson_core_jackson_annotations", "actual": "@com_fasterxml_jackson_core_jackson_annotations//jar", "bind": "jar/com/fasterxml/jackson/core/jackson_annotations"},
# duplicates in com.fasterxml.jackson.core:jackson-core promoted to 2.5.5
# - com.fasterxml.jackson.core:jackson-databind:2.5.5 wanted version 2.5.5
# - com.fasterxml.jackson.dataformat:jackson-dataformat-yaml:2.5.3 wanted version 2.5.3
# - io.circe:circe-jackson25_2.11:0.8.0 wanted version 2.5.5
    {"artifact": "com.fasterxml.jackson.core:jackson-core:2.5.5", "lang": "java", "sha1": "d0b416837b2b3907f298db2f785e9012b6881515", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_fasterxml_jackson_core_jackson_core", "actual": "@com_fasterxml_jackson_core_jackson_core//jar", "bind": "jar/com/fasterxml/jackson/core/jackson_core"},
# duplicates in com.fasterxml.jackson.core:jackson-databind promoted to 2.5.5
# - com.fasterxml.jackson.dataformat:jackson-dataformat-yaml:2.5.3 wanted version 2.5.3
# - io.circe:circe-jackson25_2.11:0.8.0 wanted version 2.5.5
    {"artifact": "com.fasterxml.jackson.core:jackson-databind:2.5.5", "lang": "java", "sha1": "b08c3194166a230e60f56ac98bcd5cab5ee39d65", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_fasterxml_jackson_core_jackson_databind", "actual": "@com_fasterxml_jackson_core_jackson_databind//jar", "bind": "jar/com/fasterxml/jackson/core/jackson_databind"},
    {"artifact": "com.fasterxml.jackson.dataformat:jackson-dataformat-yaml:2.5.3", "lang": "java", "sha1": "8fc7e5a9911c3ab4b0dd7e74f12621681835e3fc", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_fasterxml_jackson_dataformat_jackson_dataformat_yaml", "actual": "@com_fasterxml_jackson_dataformat_jackson_dataformat_yaml//jar", "bind": "jar/com/fasterxml/jackson/dataformat/jackson_dataformat_yaml"},
    {"artifact": "com.github.mpilquist:simulacrum_2.11:0.10.0", "lang": "scala", "sha1": "59bdbd0db647655e3fd9bdbaa2a362d6fe82516a", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_github_mpilquist_simulacrum_2_11", "actual": "@com_github_mpilquist_simulacrum_2_11//jar:file", "bind": "jar/com/github/mpilquist/simulacrum_2_11"},
    {"artifact": "com.google.guava:guava:18.0", "lang": "java", "sha1": "cce0823396aa693798f8882e64213b1772032b09", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_google_guava_guava", "actual": "@com_google_guava_guava//jar", "bind": "jar/com/google/guava/guava"},
    {"artifact": "commons-codec:commons-codec:1.6", "lang": "java", "sha1": "b7f0fc8f61ecadeb3695f0b9464755eee44374d4", "repository": "https://repo.maven.apache.org/maven2/", "name": "commons_codec_commons_codec", "actual": "@commons_codec_commons_codec//jar", "bind": "jar/commons_codec/commons_codec"},
    {"artifact": "io.circe:circe-core_2.11:0.8.0", "lang": "scala", "sha1": "78afb48778e2fa0077684486977597781a02cb20", "repository": "https://repo.maven.apache.org/maven2/", "name": "io_circe_circe_core_2_11", "actual": "@io_circe_circe_core_2_11//jar:file", "bind": "jar/io/circe/circe_core_2_11"},
    {"artifact": "io.circe:circe-generic_2.11:0.8.0", "lang": "scala", "sha1": "92298be2beea245b1d6f8b6a26e696c1eee842e2", "repository": "https://repo.maven.apache.org/maven2/", "name": "io_circe_circe_generic_2_11", "actual": "@io_circe_circe_generic_2_11//jar:file", "bind": "jar/io/circe/circe_generic_2_11"},
    {"artifact": "io.circe:circe-jackson25_2.11:0.8.0", "lang": "scala", "sha1": "68c7425e6380021a2517a58c91e617effa868c8b", "repository": "https://repo.maven.apache.org/maven2/", "name": "io_circe_circe_jackson25_2_11", "actual": "@io_circe_circe_jackson25_2_11//jar:file", "bind": "jar/io/circe/circe_jackson25_2_11"},
    {"artifact": "io.circe:circe-jawn_2.11:0.8.0", "lang": "scala", "sha1": "19f9f0bc116b8fe60f5d58baa8375c58125b5b10", "repository": "https://repo.maven.apache.org/maven2/", "name": "io_circe_circe_jawn_2_11", "actual": "@io_circe_circe_jawn_2_11//jar:file", "bind": "jar/io/circe/circe_jawn_2_11"},
    {"artifact": "io.circe:circe-numbers_2.11:0.8.0", "lang": "scala", "sha1": "d3e1f867a349359e4d7aa54c4d0d480dbfee05ab", "repository": "https://repo.maven.apache.org/maven2/", "name": "io_circe_circe_numbers_2_11", "actual": "@io_circe_circe_numbers_2_11//jar:file", "bind": "jar/io/circe/circe_numbers_2_11"},
    {"artifact": "io.circe:circe-parser_2.11:0.8.0", "lang": "scala", "sha1": "26126c466e0253cd5df41d7fdedb7d8c42182084", "repository": "https://repo.maven.apache.org/maven2/", "name": "io_circe_circe_parser_2_11", "actual": "@io_circe_circe_parser_2_11//jar:file", "bind": "jar/io/circe/circe_parser_2_11"},
    {"artifact": "io.get-coursier:coursier-cache_2.11:1.1.0-M1", "lang": "scala", "sha1": "dea94513d3422c6007cfb372c50733b51bf14f56", "repository": "https://repo.maven.apache.org/maven2/", "name": "io_get_coursier_coursier_cache_2_11", "actual": "@io_get_coursier_coursier_cache_2_11//jar:file", "bind": "jar/io/get_coursier/coursier_cache_2_11"},
    {"artifact": "io.get-coursier:coursier_2.11:1.1.0-M1", "lang": "scala", "sha1": "ec168a4939d60467022865ca254b2ecbbc2b6f88", "repository": "https://repo.maven.apache.org/maven2/", "name": "io_get_coursier_coursier_2_11", "actual": "@io_get_coursier_coursier_2_11//jar:file", "bind": "jar/io/get_coursier/coursier_2_11"},
    {"artifact": "org.apache.commons:commons-lang3:3.4", "lang": "java", "sha1": "5fe28b9518e58819180a43a850fbc0dd24b7c050", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_apache_commons_commons_lang3", "actual": "@org_apache_commons_commons_lang3//jar", "bind": "jar/org/apache/commons/commons_lang3"},
    {"artifact": "org.apache.httpcomponents:httpclient:4.3.5", "lang": "java", "sha1": "9783d89b8eea20a517a4afc5f979bd2882b54c44", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_apache_httpcomponents_httpclient", "actual": "@org_apache_httpcomponents_httpclient//jar", "bind": "jar/org/apache/httpcomponents/httpclient"},
    {"artifact": "org.apache.httpcomponents:httpcore:4.3.2", "lang": "java", "sha1": "31fbbff1ddbf98f3aa7377c94d33b0447c646b6e", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_apache_httpcomponents_httpcore", "actual": "@org_apache_httpcomponents_httpcore//jar", "bind": "jar/org/apache/httpcomponents/httpcore"},
    {"artifact": "org.apache.maven:maven-aether-provider:3.3.9", "lang": "java", "sha1": "29e8e7122f7a166ea53785cd75af0ef9d4d848d4", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_apache_maven_maven_aether_provider", "actual": "@org_apache_maven_maven_aether_provider//jar", "bind": "jar/org/apache/maven/maven_aether_provider"},
    {"artifact": "org.apache.maven:maven-artifact:3.3.9", "lang": "java", "sha1": "0f43afa184555fbc6e36b3334b17246c39b30f6e", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_apache_maven_maven_artifact", "actual": "@org_apache_maven_maven_artifact//jar", "bind": "jar/org/apache/maven/maven_artifact"},
    {"artifact": "org.apache.maven:maven-builder-support:3.3.9", "lang": "java", "sha1": "a96f29da7623c0e1db9824f628548fe8181f6dd0", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_apache_maven_maven_builder_support", "actual": "@org_apache_maven_maven_builder_support//jar", "bind": "jar/org/apache/maven/maven_builder_support"},
    {"artifact": "org.apache.maven:maven-model-builder:3.3.9", "lang": "java", "sha1": "e2055f9adb9f3c9a93e6b36fffe79781a785de2d", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_apache_maven_maven_model_builder", "actual": "@org_apache_maven_maven_model_builder//jar", "bind": "jar/org/apache/maven/maven_model_builder"},
    {"artifact": "org.apache.maven:maven-model:3.3.9", "lang": "java", "sha1": "6efde8cbcb4de4c47f7e9c2a3ab2806022b5c70f", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_apache_maven_maven_model", "actual": "@org_apache_maven_maven_model//jar", "bind": "jar/org/apache/maven/maven_model"},
    {"artifact": "org.apache.maven:maven-repository-metadata:3.3.9", "lang": "java", "sha1": "6850232b35e504057d67bde11efddebf6271e1ce", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_apache_maven_maven_repository_metadata", "actual": "@org_apache_maven_maven_repository_metadata//jar", "bind": "jar/org/apache/maven/maven_repository_metadata"},
    {"artifact": "org.codehaus.plexus:plexus-component-annotations:1.6", "lang": "java", "sha1": "1a34a4e12b5fded8c548a568f463dfee21500927", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_codehaus_plexus_plexus_component_annotations", "actual": "@org_codehaus_plexus_plexus_component_annotations//jar", "bind": "jar/org/codehaus/plexus/plexus_component_annotations"},
    {"artifact": "org.codehaus.plexus:plexus-interpolation:1.21", "lang": "java", "sha1": "f92de59d295f16868001644acc21720f3ec9eb15", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_codehaus_plexus_plexus_interpolation", "actual": "@org_codehaus_plexus_plexus_interpolation//jar", "bind": "jar/org/codehaus/plexus/plexus_interpolation"},
    {"artifact": "org.codehaus.plexus:plexus-utils:3.0.22", "lang": "java", "sha1": "764f26e0ab13a87c48fe55f525dfb6a133b7a92f", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_codehaus_plexus_plexus_utils", "actual": "@org_codehaus_plexus_plexus_utils//jar", "bind": "jar/org/codehaus/plexus/plexus_utils"},
# duplicates in org.eclipse.aether:aether-api fixed to 1.1.0
# - org.apache.maven:maven-aether-provider:3.3.9 wanted version 1.0.2.v20150114
# - org.eclipse.aether:aether-connector-basic:1.1.0 wanted version 1.1.0
# - org.eclipse.aether:aether-impl:1.1.0 wanted version 1.1.0
# - org.eclipse.aether:aether-spi:1.1.0 wanted version 1.1.0
# - org.eclipse.aether:aether-transport-file:1.1.0 wanted version 1.1.0
# - org.eclipse.aether:aether-transport-http:1.1.0 wanted version 1.1.0
# - org.eclipse.aether:aether-util:1.1.0 wanted version 1.1.0
    {"artifact": "org.eclipse.aether:aether-api:1.1.0", "lang": "java", "sha1": "05dd291e788f50dfb48822dab29defc16ad70860", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_eclipse_aether_aether_api", "actual": "@org_eclipse_aether_aether_api//jar", "bind": "jar/org/eclipse/aether/aether_api"},
    {"artifact": "org.eclipse.aether:aether-connector-basic:1.1.0", "lang": "java", "sha1": "f5c784bdd704ff64166c086eb6b31e2784c87b66", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_eclipse_aether_aether_connector_basic", "actual": "@org_eclipse_aether_aether_connector_basic//jar", "bind": "jar/org/eclipse/aether/aether_connector_basic"},
    {"artifact": "org.eclipse.aether:aether-impl:1.1.0", "lang": "java", "sha1": "8236fde6a1a4a7c6018d0a09e476f11c5ca8c2e1", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_eclipse_aether_aether_impl", "actual": "@org_eclipse_aether_aether_impl//jar", "bind": "jar/org/eclipse/aether/aether_impl"},
# duplicates in org.eclipse.aether:aether-spi promoted to 1.1.0
# - org.apache.maven:maven-aether-provider:3.3.9 wanted version 1.0.2.v20150114
# - org.eclipse.aether:aether-connector-basic:1.1.0 wanted version 1.1.0
# - org.eclipse.aether:aether-impl:1.1.0 wanted version 1.1.0
# - org.eclipse.aether:aether-transport-file:1.1.0 wanted version 1.1.0
# - org.eclipse.aether:aether-transport-http:1.1.0 wanted version 1.1.0
    {"artifact": "org.eclipse.aether:aether-spi:1.1.0", "lang": "java", "sha1": "a532baa7b965d2893ceace03e804b1882f448052", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_eclipse_aether_aether_spi", "actual": "@org_eclipse_aether_aether_spi//jar", "bind": "jar/org/eclipse/aether/aether_spi"},
    {"artifact": "org.eclipse.aether:aether-transport-file:1.1.0", "lang": "java", "sha1": "5c257319ccba22a02fa583c3946bfcbb47638bc8", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_eclipse_aether_aether_transport_file", "actual": "@org_eclipse_aether_aether_transport_file//jar", "bind": "jar/org/eclipse/aether/aether_transport_file"},
    {"artifact": "org.eclipse.aether:aether-transport-http:1.1.0", "lang": "java", "sha1": "5beffae29e88251ac419bf96dd1426babdb6a66f", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_eclipse_aether_aether_transport_http", "actual": "@org_eclipse_aether_aether_transport_http//jar", "bind": "jar/org/eclipse/aether/aether_transport_http"},
# duplicates in org.eclipse.aether:aether-util promoted to 1.1.0
# - org.apache.maven:maven-aether-provider:3.3.9 wanted version 1.0.2.v20150114
# - org.eclipse.aether:aether-connector-basic:1.1.0 wanted version 1.1.0
# - org.eclipse.aether:aether-impl:1.1.0 wanted version 1.1.0
# - org.eclipse.aether:aether-transport-file:1.1.0 wanted version 1.1.0
# - org.eclipse.aether:aether-transport-http:1.1.0 wanted version 1.1.0
    {"artifact": "org.eclipse.aether:aether-util:1.1.0", "lang": "java", "sha1": "d0f84ba994f0133580e8b957a44593eaa03d42cd", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_eclipse_aether_aether_util", "actual": "@org_eclipse_aether_aether_util//jar", "bind": "jar/org/eclipse/aether/aether_util"},
    {"artifact": "org.scala-sbt:test-interface:1.0", "lang": "java", "sha1": "0a3f14d010c4cb32071f863d97291df31603b521", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_scala_sbt_test_interface", "actual": "@org_scala_sbt_test_interface//jar", "bind": "jar/org/scala_sbt/test_interface"},
    {"artifact": "org.scalacheck:scalacheck_2.11:1.13.5", "lang": "scala", "sha1": "4800dfc0e73bd9af55a89ba7c8ec44c46b6f034f", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_scalacheck_scalacheck_2_11", "actual": "@org_scalacheck_scalacheck_2_11//jar:file", "bind": "jar/org/scalacheck/scalacheck_2_11"},
    {"artifact": "org.scalactic:scalactic_2.11:3.0.1", "lang": "scala", "sha1": "3c444d143879dc172fa555cea08fd0de6fa2f34f", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_scalactic_scalactic_2_11", "actual": "@org_scalactic_scalactic_2_11//jar:file", "bind": "jar/org/scalactic/scalactic_2_11"},
    {"artifact": "org.scalatest:scalatest_2.11:3.0.1", "lang": "scala", "sha1": "40a1842e7f0b915d87de1cb69f9c6962a65ee1fd", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_scalatest_scalatest_2_11", "actual": "@org_scalatest_scalatest_2_11//jar:file", "bind": "jar/org/scalatest/scalatest_2_11"},
    {"artifact": "org.slf4j:jcl-over-slf4j:1.6.2", "lang": "java", "sha1": "ac4cd2d6d0cf4342b4e8fd520c686851fc681912", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_slf4j_jcl_over_slf4j", "actual": "@org_slf4j_jcl_over_slf4j//jar", "bind": "jar/org/slf4j/jcl_over_slf4j"},
# duplicates in org.slf4j:slf4j-api promoted to 1.7.25
# - org.slf4j:jcl-over-slf4j:1.6.2 wanted version 1.6.2
# - org.slf4j:slf4j-simple:1.7.25 wanted version 1.7.25
    {"artifact": "org.slf4j:slf4j-api:1.7.25", "lang": "java", "sha1": "da76ca59f6a57ee3102f8f9bd9cee742973efa8a", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_slf4j_slf4j_api", "actual": "@org_slf4j_slf4j_api//jar", "bind": "jar/org/slf4j/slf4j_api"},
    {"artifact": "org.slf4j:slf4j-simple:1.7.25", "lang": "java", "sha1": "8dacf9514f0c707cbbcdd6fd699e8940d42fb54e", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_slf4j_slf4j_simple", "actual": "@org_slf4j_slf4j_simple//jar", "bind": "jar/org/slf4j/slf4j_simple"},
    {"artifact": "org.spire-math:jawn-parser_2.11:0.10.4", "lang": "scala", "sha1": "c0b7f7e4d4e4045b65f5b30c4f670b2dd2283b15", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_spire_math_jawn_parser_2_11", "actual": "@org_spire_math_jawn_parser_2_11//jar:file", "bind": "jar/org/spire_math/jawn_parser_2_11"},
    {"artifact": "org.spire-math:kind-projector_2.11:0.9.4", "lang": "scala", "sha1": "d8872b2c067d3c9b57bf4809d0d0ca77ed9f5435", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_spire_math_kind_projector_2_11", "actual": "@org_spire_math_kind_projector_2_11//jar:file", "bind": "jar/org/spire_math/kind_projector_2_11"},
    {"artifact": "org.typelevel:cats-core_2.11:0.9.0", "lang": "scala", "sha1": "b2f8629c6ec834d8b6321288c9fe77823f1e1314", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_typelevel_cats_core_2_11", "actual": "@org_typelevel_cats_core_2_11//jar:file", "bind": "jar/org/typelevel/cats_core_2_11"},
    {"artifact": "org.typelevel:cats-free_2.11:0.9.0", "lang": "scala", "sha1": "336ab4c4a8f4318d7546fc108099dd2d82237edc", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_typelevel_cats_free_2_11", "actual": "@org_typelevel_cats_free_2_11//jar:file", "bind": "jar/org/typelevel/cats_free_2_11"},
    {"artifact": "org.typelevel:cats-kernel_2.11:0.9.0", "lang": "scala", "sha1": "6c8cf24b5ee2c9385135cbd6d3b2ebab3262677d", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_typelevel_cats_kernel_2_11", "actual": "@org_typelevel_cats_kernel_2_11//jar:file", "bind": "jar/org/typelevel/cats_kernel_2_11"},
    {"artifact": "org.typelevel:cats-macros_2.11:0.9.0", "lang": "scala", "sha1": "be02ae0c8580815912ea52f08cc2b8b387cd202b", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_typelevel_cats_macros_2_11", "actual": "@org_typelevel_cats_macros_2_11//jar:file", "bind": "jar/org/typelevel/cats_macros_2_11"},
    {"artifact": "org.typelevel:machinist_2.11:0.6.1", "lang": "scala", "sha1": "239a56280d1cf730048f552a1a18f415bfcbf270", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_typelevel_machinist_2_11", "actual": "@org_typelevel_machinist_2_11//jar:file", "bind": "jar/org/typelevel/machinist_2_11"},
    {"artifact": "org.typelevel:macro-compat_2.11:1.1.1", "lang": "scala", "sha1": "0cb87cb74fd5fb118fede3f98075c2044616b35d", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_typelevel_macro_compat_2_11", "actual": "@org_typelevel_macro_compat_2_11//jar:file", "bind": "jar/org/typelevel/macro_compat_2_11"},
    {"artifact": "org.yaml:snakeyaml:1.12", "lang": "java", "sha1": "ebe66a6b88caab31d7a19571ad23656377523545", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_yaml_snakeyaml", "actual": "@org_yaml_snakeyaml//jar", "bind": "jar/org/yaml/snakeyaml"},
    ]

def maven_dependencies(callback = declare_maven):
    for hash in list_dependencies():
        callback(hash)
