package experiments.monadics

import scala.language.higherKinds

trait Monoid[T, M[_]] {
  def mappend[S >: T](a: M[T], b: M[S]): M[S]
}

trait Functor[A, F[_]] {
  def fmap[B](fa: F[A], f: A => B): F[B]
}

trait Monad[A, M[_]] extends Functor[A, M] {
  def flatMap[B](ma: M[A], f: A => M[B]): M[B]
}

trait MonadPlus[A, MP[_]] extends Monad[A, MP] {
  def mplus[B >: A](mpa: MP[A], other: MP[B]): MP[B]
}
