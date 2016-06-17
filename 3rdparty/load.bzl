def declare_maven(item):
  native.maven_jar(name = item["name"], artifact = item["artifact"])
