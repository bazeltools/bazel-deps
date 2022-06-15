package org.scalacheck

import org.scalacheck.rng.Seed
import cats.Monad

object GenTailRec {

  def tailRecM[A, B](a0: A)(fn: A => Gen[Either[A, B]]): Gen[B] = {
    @annotation.tailrec
    def tailRecMR(a: A, seed: Seed, labs: Set[String])(
        fn: (A, Seed) => Gen.R[Either[A, B]]
    ): Gen.R[B] = {
      val re = fn(a, seed)
      val nextLabs = labs | re.labels
      re.retrieve match {
        case None           => Gen.r(None, re.seed).copy(l = nextLabs)
        case Some(Right(b)) => Gen.r(Some(b), re.seed).copy(l = nextLabs)
        case Some(Left(a))  => tailRecMR(a, re.seed, nextLabs)(fn)
      }
    }

    // This is the "Gen.Reader-style" appoach to making a stack-safe loop:
    // we put one outer closure around an explicitly tailrec loop
    Gen.gen[B] { (p: Gen.Parameters, seed: Seed) =>
      tailRecMR(a0, seed, Set.empty) { (a, seed) => fn(a).doApply(p, seed) }
    }
  }

}

object GenMonad extends Monad[Gen] {
  def pure[A](a: A): Gen[A] = Gen.const(a)
  override def map[A, B](g: Gen[A])(fn: A => B): Gen[B] = g.map(fn)
  def flatMap[A, B](g: Gen[A])(fn: A => Gen[B]): Gen[B] = g.flatMap(fn)
  def tailRecM[A, B](a: A)(fn: A => Gen[Either[A, B]]): Gen[B] =
    GenTailRec.tailRecM(a)(fn)
}
