package com.github.johnynek.bazel_deps

import cats.{Foldable, Reducible}
import cats.data.NonEmptyList
import scala.util.{Failure, Success, Try}
import scala.collection.immutable.SortedMap

import cats.syntax.all._

trait TryMerge[T] { self =>
  def tryMerge(debugName: Option[String], left: T, right: T): Try[T]
  def reverse: TryMerge[T] =
    new TryMerge[T] {
      def tryMerge(debugName: Option[String], left: T, right: T): Try[T] = self.tryMerge(debugName, right, left)
      override def reverse = self
    }
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
          case (l @ Some(_), None)    => Success(l)
          case (None, r @ Some(_))    => Success(r)
        }
      }
    }

  implicit def tryStringMapMerge[T: TryMerge]: TryMerge[SortedMap[String, T]] =
    new TryMerge[SortedMap[String, T]] {
      def tryMerge(
        debugName: Option[String],
          left: SortedMap[String, T],
          right: SortedMap[String, T]
      ): Try[SortedMap[String, T]] = {
        val tmt = implicitly[TryMerge[T]]
        val (big, small, tm) =
          if (right.size > left.size) (right, left, tmt.reverse)
          else (left, right, tmt)

        small.toStream
          .foldM(big) { case (acc, (k, rv)) =>
            acc.get(k) match {
              case Some(lv) =>
                val nextDebug = Some(debugName.fold(k) {p => s"$p:$k"})
                tm.tryMerge(nextDebug, lv, rv).map(acc.updated(k, _))
              case None =>
                Success(acc.updated(k, rv))
            }  
          }
      }
    }
}

