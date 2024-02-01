package com.github.johnynek.bazel_deps

import cats.{Foldable, Reducible}
import cats.data.NonEmptyList
import scala.util.{Failure, Success, Try}

import cats.syntax.all._

trait TryMerge[T] {
  def tryMerge(debugName: Option[String], left: T, right: T): Try[T]
}

object TryMerge {
  def tryMerge[T: TryMerge](debugName: Option[String], a: T, b: T): Try[T] =
    implicitly[TryMerge[T]].tryMerge(debugName, a, b)

  def tryMergeAll[F[_]: Reducible, A: TryMerge](debugName: Option[String], r: F[A]): Try[A] =
    r.reduceLeftM(Success(_): Try[A])(tryMerge(debugName, _, _))

  def tryMergeAllOr[F[_]: Foldable, A: TryMerge](debugName: Option[String], r: F[A], res: => Try[A]): Try[A] =
    NonEmptyList.fromList(r.toList) match {
      case Some(nel) => tryMergeAll[NonEmptyList, A](debugName, nel)
      case None => res
    }

  implicit def tryOptMerge[T: TryMerge]: TryMerge[Option[T]] =
    new TryMerge[Option[T]] {
      def tryMerge(debugName: Option[String], left: Option[T], right: Option[T]): Try[Option[T]] = {
        (left, right) match {
          case (None, None)       => Success(None)
          case (Some(l), Some(r)) => TryMerge.tryMerge(debugName, l, r).map(Some(_))
          case (Some(l), None)    => Success(Some(l))
          case (None, Some(r))    => Success(Some(r))
        }
      }
    }

  implicit def tryStringMapMerge[T: TryMerge]: TryMerge[Map[String, T]] =
    new TryMerge[Map[String, T]] {
      def tryMerge(
        debugName: Option[String],
          left: Map[String, T],
          right: Map[String, T]
      ): Try[Map[String, T]] = {
        (left.keySet | right.keySet).toList.sorted.foldM(Map.empty[String, T]) {
          (m, nextK) =>
            val r: Try[T] =
              (left.get(nextK), right.get(nextK)) match {
                case (Some(l), None)    => Success(l)
                case (None, Some(r))    => Success(r)
                case (Some(l), Some(r)) =>
                  TryMerge.tryMerge(Some(debugName.map{p => s"$p:$nextK"}.getOrElse(nextK)), l, r)
                case (None, None) =>
                    Failure(new Exception(s"Shouldn't happen, key=$nextK was in keyset"))
              }
            r.map { innerV =>
              m + ((nextK, innerV))
            }
        }
      }
    }
}

