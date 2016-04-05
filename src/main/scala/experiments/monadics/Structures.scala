package experiments.monadics

import scala.language.higherKinds

trait Monoid[T, M[_]] {
  def mappend(a: M[T], b: M[T]): M[T]
}

trait Functor[A, F[_]] {
  def fmap[B](fa: F[A], f: A => B): F[B]
}

trait Monad[A, M[_]] extends Functor[A, M] {
  def flatMap[B](ma: M[A], f: A => M[B]): M[B]
}

trait MonadPlus[A, MP[_]] extends Monad[A, MP] {
  def mplus(mpa: MP[A], other: MP[A]): MP[A]
}
