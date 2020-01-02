package com.github.johnynek.bazel_deps

import java.net.{URL, URLConnection, URLStreamHandler}

final class S3Handler extends URLStreamHandler {
    def openConnection(url: URL): URLConnection = new S3URLConnection(url)
}