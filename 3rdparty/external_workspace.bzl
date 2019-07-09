# Do not edit. bazel-deps autogenerates this file from dependencies.yaml.
def _build_external_workspace_impl(ctx):

    target_configs = ctx.attr.target_configs

    result_dict = {}
    for key, cfg in target_configs.items():
      build_file_to_target_name = key.split(":")
      build_file = build_file_to_target_name[0]
      target_name = build_file_to_target_name[1]
      if build_file not in result_dict:
        result_dict[build_file] = []
      result_dict[build_file].append(cfg)

    for key, cfg in result_dict.items():
      build_file_contents = 'load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")\n\n'
      for entry in cfg:
        if entry[2] == "java":
            exports = ""
            for e in entry[4].split("|||"):
              exports += "\"" + e + "\","
            build_file_contents += """
java_library(
    name = "{name}",
    exports = [
        {exports}
    ],
    visibility = [
        "{visibility}"
    ]
)
\n""".format(name = entry[0], exports=exports, visibility=entry[7])
      ctx.file(ctx.path(key + "/BUILD"), build_file_contents, False)
    return None

build_external_workspace = repository_rule(
    attrs = {
        "target_configs": attr.string_list_dict(mandatory = True),
        "separator": attr.string(mandatory = True),
    },
    implementation = _build_external_workspace_impl
)


