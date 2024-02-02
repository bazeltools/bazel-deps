package com.github.johnynek.bazel_deps

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.{Prop, Gen, Arbitrary}
import cats.{Semigroup, Eq}
import scala.util.{Success, Failure}
import scala.collection.immutable.SortedMap

import TryMerge.tryMerge
import java.util.concurrent.Semaphore

class TryMergeTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  def associative[A: TryMerge: Eq](a: A, b: A, c: A) = {
    val right = tryMerge(None, b, c).flatMap(tryMerge(None, a, _))
    val left = tryMerge(None, a, b).flatMap(tryMerge(None, _, c))

    (left, right) match {
      case (Success(l), Success(r)) => assert(Eq.eqv(l, r))
      case (Failure(_), Failure(_)) => ()
      case mismatch => fail(s"associative mismatch: $a, $b, $c => $mismatch")
    }
  }

  implicit val tryMergeI: TryMerge[Int] =
    new TryMerge[Int] {
      def tryMerge(debug: Option[String], a: Int, b: Int) =
        // take the high 16 bits of a and low 16 bits of b
        // this is associative but not commutative
        Success((a & 0xFFFF0000) | (b & 0x0000FFFF))
    }

  def revTry[A: TryMerge]: TryMerge[A] = implicitly[TryMerge[A]].reverse
  
  property("try merge is associative Int") {
    forAll { (a: Int, b: Int, c: Int) =>
      associative(a, b, c)
      associative(a, b, c)(revTry, implicitly)
    }
  }


  property("try merge is associative Option[Int]") {
    forAll { (a: Option[Int], b: Option[Int], c: Option[Int]) =>
      associative(a, b, c)
      associative(a, b, c)(revTry, implicitly)
    }
  }

  property("try merge is associative SortedMap[String, Int]") {
    forAll { (a: SortedMap[String, Int], b: SortedMap[String, Int], c: SortedMap[String, Int]) =>
      associative(a, b, c)
      associative(a, b, c)(revTry, implicitly)
    }
  }

  property("try merge with taking this rhs is the same as ++ on SortedMap") {
    forAll { (a: SortedMap[String, Int], b: SortedMap[String, Int]) =>
      implicit val rhs = new TryMerge[Int] {
        def tryMerge(d: Option[String], a: Int, b: Int) = Success(b)
      }
      assert(tryMerge(None, a, b) == Success(a ++ b))
    }
  }

  property("if we always return it's the same as a Semigroup") {
    forAll { (a: SortedMap[String, Int], b: SortedMap[String, Int]) =>
      implicit val sgBits = new Semigroup[Int] {
        def combine(a: Int, b: Int) = (a & 0xFFFF0000) | (b & 0x0000FFFF)
      }
      val sm = new Semigroup[SortedMap[String, Int]] {
        def combine(a: SortedMap[String, Int], b: SortedMap[String, Int]) = {
          val allKeys = a.keySet | b.keySet
          SortedMap(allKeys.iterator.map { k =>
            (a.get(k), b.get(k)) match {
              case (Some(l), Some(r)) => (k, sgBits.combine(l, r))
              case (Some(l), None) => (k, l)
              case (None, Some(r)) => (k, r)
              case (None, None) => sys.error(s"unreachable: non-sense key: $k")
            }
          }.toList: _*)
        }
      }
      assert(tryMerge(None, a, b) == Success(sm.combine(a, b)))
    }
  }
}
