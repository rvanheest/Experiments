package experiments.monadics

import experiments.monadics.instances.{Just, Maybe, None}

import scala.language.higherKinds

trait Monoid[M[_]] {
  def mappend[T, S >: T](a: M[T], b: M[S]): M[S]
}

trait Functor[F[_]] {
  def map[A, B](f: A => B, functor: F[A]): F[B]
}

trait Applicative[App[_]] extends Functor[App] {
  def create[A](a: A): App[A]

  def <*>[A, B](appFunc: App[A => B], appA: App[A]): App[B]

  def *>[A, B](appA: App[A], appB: App[B]): App[B] = {
    <*>(map[A, B => B](_ => identity[B], appA), appB)
  }

  def <*[A, B](appA: App[A], appB: App[B]): App[A] = {
    <*>(map[A, B => A](a => _ => a, appA), appB)
  }

  def <**>[A, B](appA: App[A], appFunc: App[A => B]): App[B] = {
    <*>(appFunc, appA)
  }
}

trait Monad[M[_]] extends Applicative[M] {
  override def <*>[A, B](mFunc: M[A => B], mA: M[A]): M[B] = {
    flatMap[A, B](mA, a => map[A => B, B](f => f(a), mFunc))
  }

  override def *>[A, B](mA: M[A], mB: M[B]): M[B] = andThen(mA, mB)

  override def <*[A, B](mA: M[A], mB: M[B]): M[A] = thenAnd(mA, mB)

  def flatMap[A, B](monad: M[A], f: A => M[B]): M[B]

  def andThen[A, B](mA: M[A], mB: M[B]): M[B] = flatMap[A, B](mA, a => mB)

  def thenAnd[A, B](mA: M[A], mB: M[B]): M[A] = flatMap[A, A](mA, a => map[B, A](_ => a, mB))

  def flatten[A, B](mA: M[A])(implicit ev: A <:< M[B]): M[B] = flatMap(mA, ev)
}

trait Alternative[Alt[_]] extends Applicative[Alt] {
  def empty[A]: Alt[A]

  def getOrElse[A, B >: A](alt: Alt[A], default: => B): B

  def orElse[A, B >: A](alt1: Alt[A], alt2: => Alt[B]): Alt[B]

  def some[A](alt: Alt[A]): Alt[List[A]] = some_v(alt)

  def many[A](alt: Alt[A]): Alt[List[A]] = many_v(alt)

  private def many_v[A](alt: Alt[A]): Alt[List[A]] = orElse(some_v(alt), create(Nil))

  private def some_v[A](alt: Alt[A]): Alt[List[A]] = {
    <*>(map[A, List[A] => List[A]](a => a :: _, alt), many_v(alt))
  }

  def maybe[A](alt: Alt[A]): Alt[Maybe[A]] = {
    orElse(map[A, Maybe[A]](Just(_), alt), create[Maybe[A]](None))
  }
}

trait MonadPlus[MP[_]] extends Monad[MP] with Alternative[MP] {
  def mplus[A, B >: A](mp1: MP[A], mp2: MP[B]): MP[B] = {
    orElse(mp1, mp2)
  }

  def filter[A](predicate: A => Boolean, mp: MP[A]): MP[A] = {
    flatMap[A, A](mp, a => if (predicate(a)) create(a) else empty[A])
  }
}
