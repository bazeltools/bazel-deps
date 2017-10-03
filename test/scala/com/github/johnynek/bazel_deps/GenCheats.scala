package org.scalacheck

import org.scalacheck.rng.Seed
import cats.Monad

object GenTailRec {
  @annotation.tailrec
  def tailRecMR[A, B](a: A)(fn: A => Gen.R[Either[A, B]]): Gen.R[B] = {
    val re = fn(a)
    re.retrieve match {
      case None => Gen.r(None, re.seed).copy(l = re.labels)
      case Some(Left(a)) => tailRecMR(a)(fn)
      case Some(Right(b)) => Gen.r(Some(b), re.seed).copy(l = re.labels)
    }
  }

  def tailRecM[A, B](a: A)(fn: A => Gen[Either[A, B]]): Gen[B] =
    Gen.gen[B] { (p: Gen.Parameters, seed: Seed) =>
      tailRecMR(a) { a => fn(a).doApply(p, seed) }
    }
}

object GenMonad extends Monad[Gen] {
  def pure[A](a: A): Gen[A] = Gen.const(a)
  override def map[A, B](g: Gen[A])(fn: A => B): Gen[B] = g.map(fn)
  def flatMap[A, B](g: Gen[A])(fn: A => Gen[B]): Gen[B] = g.flatMap(fn)
  def tailRecM[A, B](a: A)(fn: A => Gen[Either[A, B]]): Gen[B] =
    GenTailRec.tailRecM(a)(fn)
}
