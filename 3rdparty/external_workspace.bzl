# Do not edit. bazel-deps autogenerates this file from dependencies.yaml.
_JAVA_LIBRARY_TEMPLATE = """
java_library(
  name = "{name}",
  exports = [
      {exports}
  ],
  visibility = [
      "{visibility}"
  ]
)\n"""

def _build_external_workspace_impl(ctx):
    separator = ctx.attr.separator
    target_configs = ctx.attr.target_configs

    result_dict = {}
    for key, cfg in target_configs.items():
      build_file_to_target_name = key.split(":")
      build_file = build_file_to_target_name[0]
      target_name = build_file_to_target_name[1]
      if build_file not in result_dict:
        result_dict[build_file] = []
      result_dict[build_file].append(cfg)

    for key, file_entries in result_dict.items():
      build_file_contents = 'load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")\n\n'
      for build_target in file_entries:
        entry_map = {}
        for entry in build_target:
          elements = entry.split(separator)
          build_entry_key = elements[0]
          if elements[1] == "L":
            entry_map[build_entry_key] = elements[2::]
          elif elements[1] == "B":
            entry_map[build_entry_key] = (elements[2] == "true" or elements[2] == "True")
          else:
            entry_map[build_entry_key] = elements[2]
        if "name" not in entry_map:
          print(file_entries)
          print(entry_map)
          print(entry)
          print(key)
        if entry_map["lang"] == "java":
            exports = ""
            for e in entry_map.get("exports", []):
              exports += "\"" + e + "\","
            build_file_contents += _JAVA_LIBRARY_TEMPLATE.format(name = entry_map["name"], exports=exports, visibility=entry_map["visibility"])
      ctx.file(ctx.path(key + "/BUILD"), build_file_contents, False)
    return None

build_external_workspace = repository_rule(
    attrs = {
        "target_configs": attr.string_list_dict(mandatory = True),
        "separator": attr.string(mandatory = True),
    },
    implementation = _build_external_workspace_impl
)


