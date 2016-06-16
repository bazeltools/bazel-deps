package com.github.johnynek.bazel_deps

class Normalizer {
  type Failure = Map[UnversionedCoordinate, Set[(MavenCoordinate, Explicitness)]]
  type Success = Graph[MavenCoordinate, Unit]

  def normalize(deps: Graph[(MavenCoordinate, Explicitness), Unit], opts: Options): Either[Failure, Success] = ???
}
