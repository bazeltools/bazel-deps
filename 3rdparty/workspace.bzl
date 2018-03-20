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
    {"artifact": "asm:asm:3.3.1", "lang": "java", "sha1": "1d5f20b4ea675e6fab6ab79f1cd60ec268ddc015", "repository": "https://repo.maven.apache.org/maven2/", "name": "asm_asm", "actual": "@asm_asm//jar", "bind": "jar/asm/asm"},
    {"artifact": "com.chuusai:shapeless_2.11:2.3.2", "lang": "scala", "sha1": "f40ed6e303d550293f5f8f3743681d98e31f2360", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_chuusai_shapeless_2_11", "actual": "@com_chuusai_shapeless_2_11//jar:file", "bind": "jar/com/chuusai/shapeless_2_11"},
    {"artifact": "com.fasterxml.jackson.core:jackson-annotations:2.5.0", "lang": "java", "sha1": "a2a55a3375bc1cef830ca426d68d2ea22961190e", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_fasterxml_jackson_core_jackson_annotations", "actual": "@com_fasterxml_jackson_core_jackson_annotations//jar", "bind": "jar/com/fasterxml/jackson/core/jackson_annotations"},
# duplicates in com.fasterxml.jackson.core:jackson-core promoted to 2.5.5
# - com.fasterxml.jackson.dataformat:jackson-dataformat-yaml:2.5.3 wanted version 2.5.3
# - io.circe:circe-jackson25_2.11:0.8.0 wanted version 2.5.5
    {"artifact": "com.fasterxml.jackson.core:jackson-core:2.5.5", "lang": "java", "sha1": "d0b416837b2b3907f298db2f785e9012b6881515", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_fasterxml_jackson_core_jackson_core", "actual": "@com_fasterxml_jackson_core_jackson_core//jar", "bind": "jar/com/fasterxml/jackson/core/jackson_core"},
# duplicates in com.fasterxml.jackson.core:jackson-databind promoted to 2.5.5
# - com.fasterxml.jackson.dataformat:jackson-dataformat-yaml:2.5.3 wanted version 2.5.3
# - io.circe:circe-jackson25_2.11:0.8.0 wanted version 2.5.5
    {"artifact": "com.fasterxml.jackson.core:jackson-databind:2.5.5", "lang": "java", "sha1": "b08c3194166a230e60f56ac98bcd5cab5ee39d65", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_fasterxml_jackson_core_jackson_databind", "actual": "@com_fasterxml_jackson_core_jackson_databind//jar", "bind": "jar/com/fasterxml/jackson/core/jackson_databind"},
    {"artifact": "com.fasterxml.jackson.dataformat:jackson-dataformat-yaml:2.5.3", "lang": "java", "sha1": "8fc7e5a9911c3ab4b0dd7e74f12621681835e3fc", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_fasterxml_jackson_dataformat_jackson_dataformat_yaml", "actual": "@com_fasterxml_jackson_dataformat_jackson_dataformat_yaml//jar", "bind": "jar/com/fasterxml/jackson/dataformat/jackson_dataformat_yaml"},
    {"artifact": "com.github.mpilquist:simulacrum_2.11:0.10.0", "lang": "scala", "sha1": "59bdbd0db647655e3fd9bdbaa2a362d6fe82516a", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_github_mpilquist_simulacrum_2_11", "actual": "@com_github_mpilquist_simulacrum_2_11//jar:file", "bind": "jar/com/github/mpilquist/simulacrum_2_11"},
    {"artifact": "com.google.code.findbugs:jsr305:1.3.9", "lang": "java", "sha1": "40719ea6961c0cb6afaeb6a921eaa1f6afd4cfdf", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_google_code_findbugs_jsr305", "actual": "@com_google_code_findbugs_jsr305//jar", "bind": "jar/com/google/code/findbugs/jsr305"},
    {"artifact": "com.google.guava:guava:11.0.2", "lang": "java", "sha1": "35a3c69e19d72743cac83778aecbee68680f63eb", "repository": "https://repo.maven.apache.org/maven2/", "name": "com_google_guava_guava", "actual": "@com_google_guava_guava//jar", "bind": "jar/com/google/guava/guava"},
    {"artifact": "commons-codec:commons-codec:1.6", "lang": "java", "sha1": "b7f0fc8f61ecadeb3695f0b9464755eee44374d4", "repository": "https://repo.maven.apache.org/maven2/", "name": "commons_codec_commons_codec", "actual": "@commons_codec_commons_codec//jar", "bind": "jar/commons_codec/commons_codec"},
    {"artifact": "io.circe:circe-core_2.11:0.8.0", "lang": "scala", "sha1": "78afb48778e2fa0077684486977597781a02cb20", "repository": "https://repo.maven.apache.org/maven2/", "name": "io_circe_circe_core_2_11", "actual": "@io_circe_circe_core_2_11//jar:file", "bind": "jar/io/circe/circe_core_2_11"},
    {"artifact": "io.circe:circe-generic_2.11:0.8.0", "lang": "scala", "sha1": "92298be2beea245b1d6f8b6a26e696c1eee842e2", "repository": "https://repo.maven.apache.org/maven2/", "name": "io_circe_circe_generic_2_11", "actual": "@io_circe_circe_generic_2_11//jar:file", "bind": "jar/io/circe/circe_generic_2_11"},
    {"artifact": "io.circe:circe-jackson25_2.11:0.8.0", "lang": "scala", "sha1": "68c7425e6380021a2517a58c91e617effa868c8b", "repository": "https://repo.maven.apache.org/maven2/", "name": "io_circe_circe_jackson25_2_11", "actual": "@io_circe_circe_jackson25_2_11//jar:file", "bind": "jar/io/circe/circe_jackson25_2_11"},
    {"artifact": "io.circe:circe-jawn_2.11:0.8.0", "lang": "java", "sha1": "19f9f0bc116b8fe60f5d58baa8375c58125b5b10", "repository": "https://repo.maven.apache.org/maven2/", "name": "io_circe_circe_jawn_2_11", "actual": "@io_circe_circe_jawn_2_11//jar", "bind": "jar/io/circe/circe_jawn_2_11"},
    {"artifact": "io.circe:circe-numbers_2.11:0.8.0", "lang": "java", "sha1": "d3e1f867a349359e4d7aa54c4d0d480dbfee05ab", "repository": "https://repo.maven.apache.org/maven2/", "name": "io_circe_circe_numbers_2_11", "actual": "@io_circe_circe_numbers_2_11//jar", "bind": "jar/io/circe/circe_numbers_2_11"},
    {"artifact": "io.circe:circe-parser_2.11:0.8.0", "lang": "scala", "sha1": "26126c466e0253cd5df41d7fdedb7d8c42182084", "repository": "https://repo.maven.apache.org/maven2/", "name": "io_circe_circe_parser_2_11", "actual": "@io_circe_circe_parser_2_11//jar:file", "bind": "jar/io/circe/circe_parser_2_11"},
    {"artifact": "javax.annotation:jsr250-api:1.0", "lang": "java", "sha1": "5025422767732a1ab45d93abfea846513d742dcf", "repository": "https://repo.maven.apache.org/maven2/", "name": "javax_annotation_jsr250_api", "actual": "@javax_annotation_jsr250_api//jar", "bind": "jar/javax/annotation/jsr250_api"},
    {"artifact": "javax.enterprise:cdi-api:1.0", "lang": "java", "sha1": "44c453f60909dfc223552ace63e05c694215156b", "repository": "https://repo.maven.apache.org/maven2/", "name": "javax_enterprise_cdi_api", "actual": "@javax_enterprise_cdi_api//jar", "bind": "jar/javax/enterprise/cdi_api"},
    {"artifact": "org.apache.httpcomponents:httpclient:4.2.6", "lang": "java", "sha1": "e4ca30a6a3a075053a61c6fc850d2432dc012ba7", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_apache_httpcomponents_httpclient", "actual": "@org_apache_httpcomponents_httpclient//jar", "bind": "jar/org/apache/httpcomponents/httpclient"},
    {"artifact": "org.apache.httpcomponents:httpcore:4.2.5", "lang": "java", "sha1": "472f0f5f8dba5d1962cb9d7739feed739a31c30d", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_apache_httpcomponents_httpcore", "actual": "@org_apache_httpcomponents_httpcore//jar", "bind": "jar/org/apache/httpcomponents/httpcore"},
    {"artifact": "org.apache.maven:maven-aether-provider:3.1.0", "lang": "java", "sha1": "dda2231a2be2768109d474805c702b76a8e794e6", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_apache_maven_maven_aether_provider", "actual": "@org_apache_maven_maven_aether_provider//jar", "bind": "jar/org/apache/maven/maven_aether_provider"},
    {"artifact": "org.apache.maven:maven-model-builder:3.1.0", "lang": "java", "sha1": "13ba294cedb659c3851f0c2980af7f44bcc6a8e0", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_apache_maven_maven_model_builder", "actual": "@org_apache_maven_maven_model_builder//jar", "bind": "jar/org/apache/maven/maven_model_builder"},
    {"artifact": "org.apache.maven:maven-model:3.1.0", "lang": "java", "sha1": "82b2f097c1cc9a8d0e6b99af5e56327d5002c30f", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_apache_maven_maven_model", "actual": "@org_apache_maven_maven_model//jar", "bind": "jar/org/apache/maven/maven_model"},
    {"artifact": "org.apache.maven:maven-repository-metadata:3.1.0", "lang": "java", "sha1": "77bb2c383b1654b158cf9f905f4105d9d522fc7e", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_apache_maven_maven_repository_metadata", "actual": "@org_apache_maven_maven_repository_metadata//jar", "bind": "jar/org/apache/maven/maven_repository_metadata"},
    {"artifact": "org.codehaus.plexus:plexus-classworlds:2.4.2", "lang": "java", "sha1": "e006f28662eba33d91d1c5e342e0bd66f8e9da18", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_codehaus_plexus_plexus_classworlds", "actual": "@org_codehaus_plexus_plexus_classworlds//jar", "bind": "jar/org/codehaus/plexus/plexus_classworlds"},
    {"artifact": "org.codehaus.plexus:plexus-component-annotations:1.5.5", "lang": "java", "sha1": "c72f2660d0cbed24246ddb55d7fdc4f7374d2078", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_codehaus_plexus_plexus_component_annotations", "actual": "@org_codehaus_plexus_plexus_component_annotations//jar", "bind": "jar/org/codehaus/plexus/plexus_component_annotations"},
    {"artifact": "org.codehaus.plexus:plexus-interpolation:1.16", "lang": "java", "sha1": "a868d4a603bd42c9dee67890c4e60e360a11838c", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_codehaus_plexus_plexus_interpolation", "actual": "@org_codehaus_plexus_plexus_interpolation//jar", "bind": "jar/org/codehaus/plexus/plexus_interpolation"},
    {"artifact": "org.codehaus.plexus:plexus-utils:3.0.10", "lang": "java", "sha1": "65e6460a49460d2ca038f8644ff9ae6d878733b8", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_codehaus_plexus_plexus_utils", "actual": "@org_codehaus_plexus_plexus_utils//jar", "bind": "jar/org/codehaus/plexus/plexus_utils"},
# duplicates in org.eclipse.aether:aether-api fixed to 1.0.2.v20150114
# - org.apache.maven:maven-aether-provider:3.1.0 wanted version 0.9.0.M2
# - org.eclipse.aether:aether-connector-basic:1.0.2.v20150114 wanted version 1.0.2.v20150114
# - org.eclipse.aether:aether-impl:1.0.2.v20150114 wanted version 1.0.2.v20150114
# - org.eclipse.aether:aether-transport-file:1.0.2.v20150114 wanted version 1.0.2.v20150114
# - org.eclipse.aether:aether-transport-http:1.0.2.v20150114 wanted version 1.0.2.v20150114
    {"artifact": "org.eclipse.aether:aether-api:1.0.2.v20150114", "lang": "java", "sha1": "839f93a5213fb3e233b09bfd6d6b95669f7043c0", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_eclipse_aether_aether_api", "actual": "@org_eclipse_aether_aether_api//jar", "bind": "jar/org/eclipse/aether/aether_api"},
    {"artifact": "org.eclipse.aether:aether-connector-basic:1.0.2.v20150114", "lang": "java", "sha1": "d55c03b16efc16f25e1fd9fe0f37878fddbeed68", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_eclipse_aether_aether_connector_basic", "actual": "@org_eclipse_aether_aether_connector_basic//jar", "bind": "jar/org/eclipse/aether/aether_connector_basic"},
    {"artifact": "org.eclipse.aether:aether-impl:1.0.2.v20150114", "lang": "java", "sha1": "f147539e6e60dfbda9ef7f6d750066170f61b7a1", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_eclipse_aether_aether_impl", "actual": "@org_eclipse_aether_aether_impl//jar", "bind": "jar/org/eclipse/aether/aether_impl"},
# duplicates in org.eclipse.aether:aether-spi promoted to 1.0.2.v20150114
# - org.apache.maven:maven-aether-provider:3.1.0 wanted version 0.9.0.M2
# - org.eclipse.aether:aether-connector-basic:1.0.2.v20150114 wanted version 1.0.2.v20150114
# - org.eclipse.aether:aether-impl:1.0.2.v20150114 wanted version 1.0.2.v20150114
# - org.eclipse.aether:aether-transport-file:1.0.2.v20150114 wanted version 1.0.2.v20150114
# - org.eclipse.aether:aether-transport-http:1.0.2.v20150114 wanted version 1.0.2.v20150114
    {"artifact": "org.eclipse.aether:aether-spi:1.0.2.v20150114", "lang": "java", "sha1": "8428dfa330107984f3e3ac05cc3ebd50b2676866", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_eclipse_aether_aether_spi", "actual": "@org_eclipse_aether_aether_spi//jar", "bind": "jar/org/eclipse/aether/aether_spi"},
    {"artifact": "org.eclipse.aether:aether-transport-file:1.0.2.v20150114", "lang": "java", "sha1": "79ffcce2aa9c525ad5a1d0fc3f9669133c3f9572", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_eclipse_aether_aether_transport_file", "actual": "@org_eclipse_aether_aether_transport_file//jar", "bind": "jar/org/eclipse/aether/aether_transport_file"},
    {"artifact": "org.eclipse.aether:aether-transport-http:1.0.2.v20150114", "lang": "java", "sha1": "262b7fb2e9872f470c059bc4053a3f2f7449932d", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_eclipse_aether_aether_transport_http", "actual": "@org_eclipse_aether_aether_transport_http//jar", "bind": "jar/org/eclipse/aether/aether_transport_http"},
# duplicates in org.eclipse.aether:aether-util promoted to 1.0.2.v20150114
# - org.apache.maven:maven-aether-provider:3.1.0 wanted version 0.9.0.M2
# - org.eclipse.aether:aether-connector-basic:1.0.2.v20150114 wanted version 1.0.2.v20150114
# - org.eclipse.aether:aether-impl:1.0.2.v20150114 wanted version 1.0.2.v20150114
# - org.eclipse.aether:aether-transport-file:1.0.2.v20150114 wanted version 1.0.2.v20150114
# - org.eclipse.aether:aether-transport-http:1.0.2.v20150114 wanted version 1.0.2.v20150114
    {"artifact": "org.eclipse.aether:aether-util:1.0.2.v20150114", "lang": "java", "sha1": "d2d3c74a5210544b5cdce89a2c1d1c62835692d1", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_eclipse_aether_aether_util", "actual": "@org_eclipse_aether_aether_util//jar", "bind": "jar/org/eclipse/aether/aether_util"},
    {"artifact": "org.eclipse.sisu:org.eclipse.sisu.inject:0.0.0.M2a", "lang": "java", "sha1": "17941e32c751179a9628b25f54ce5641edafb9be", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_eclipse_sisu_org_eclipse_sisu_inject", "actual": "@org_eclipse_sisu_org_eclipse_sisu_inject//jar", "bind": "jar/org/eclipse/sisu/org_eclipse_sisu_inject"},
    {"artifact": "org.eclipse.sisu:org.eclipse.sisu.plexus:0.0.0.M2a", "lang": "java", "sha1": "07510dc8dfe27a0b57c17601bc760b7b0c8f95fa", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_eclipse_sisu_org_eclipse_sisu_plexus", "actual": "@org_eclipse_sisu_org_eclipse_sisu_plexus//jar", "bind": "jar/org/eclipse/sisu/org_eclipse_sisu_plexus"},
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
    {"artifact": "org.spire-math:jawn-parser_2.11:0.10.4", "lang": "java", "sha1": "c0b7f7e4d4e4045b65f5b30c4f670b2dd2283b15", "repository": "https://repo.maven.apache.org/maven2/", "name": "org_spire_math_jawn_parser_2_11", "actual": "@org_spire_math_jawn_parser_2_11//jar", "bind": "jar/org/spire_math/jawn_parser_2_11"},
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
