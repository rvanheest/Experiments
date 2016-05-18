package experiments.monadics

import experiments.monadics.instances.{Just, Maybe, None}

import scala.language.higherKinds

trait Monoid[M[_]] {
  def mappend[T, S >: T](a: M[T], b: M[S]): M[S]
}

trait Functor[F[_]] {
  def map[A, B](functor: F[A])(f: A => B): F[B]
}

trait Applicative[App[_]] extends Functor[App] {
  def create[A](a: A): App[A]

  def <*>[A, B](appFunc: App[A => B], appA: App[A]): App[B]

  def *>[A, B](appA: App[A], appB: App[B]): App[B] = {
    <*>(map[A, B => B](appA)(_ => identity[B]), appB)
  }

  def <*[A, B](appA: App[A], appB: App[B]): App[A] = {
    <*>(map[A, B => A](appA)(a => _ => a), appB)
  }

  def <**>[A, B](appA: App[A], appFunc: App[A => B]): App[B] = {
    <*>(appFunc, appA)
  }
}

trait Monad[M[_]] extends Applicative[M] {
  override def <*>[A, B](mFunc: M[A => B], mA: M[A]): M[B] = {
    flatMap(mA)(a => map(mFunc)(_(a)))
  }

  override def *>[A, B](mA: M[A], mB: M[B]): M[B] = {
    andThen(mA, mB)
  }

  override def <*[A, B](mA: M[A], mB: M[B]): M[A] = {
    thenAnd(mA, mB)
  }

  def flatMap[A, B](monad: M[A])(f: A => M[B]): M[B]

  def andThen[A, B](mA: M[A], mB: M[B]): M[B] = {
    flatMap(mA)(a => mB)
  }

  def thenAnd[A, B](mA: M[A], mB: M[B]): M[A] = {
    flatMap(mA)(a => map(mB)(_ => a))
  }

  def flatten[A, B](mA: M[A])(implicit ev: A <:< M[B]): M[B] = {
    flatMap(mA)(ev)
  }
}

trait Alternative[Alt[_]] extends Applicative[Alt] {
  def empty[A]: Alt[A]

  def getOrElse[A, B >: A](alt: Alt[A], default: => B): B

  def orElse[A, B >: A](alt1: Alt[A], alt2: => Alt[B]): Alt[B]

  def atLeastOnce[A](alt: Alt[A]): Alt[List[A]] = {
    atLeastOnce_v(alt)
  }

  def many[A](alt: Alt[A]): Alt[List[A]] = {
    many_v(alt)
  }

  private def many_v[A](alt: Alt[A]): Alt[List[A]] = {
    orElse(atLeastOnce_v(alt), create(Nil))
  }

  private def atLeastOnce_v[A](alt: Alt[A]): Alt[List[A]] = {
    <*>(map[A, List[A] => List[A]](alt)(a => a :: _), many_v(alt))
  }

  def maybe[A](alt: Alt[A]): Alt[Maybe[A]] = {
    orElse(map(alt)(Just(_)), create(None))
  }
}

trait MonadPlus[MP[_]] extends Monad[MP] with Alternative[MP] {
  def mplus[A, B >: A](mp1: MP[A], mp2: MP[B]): MP[B] = {
    orElse(mp1, mp2)
  }

  def filter[A](mp: MP[A])(predicate: A => Boolean): MP[A] = {
    flatMap(mp)(a => if (predicate(a)) create(a) else empty[A])
  }

  def filterNot[A](mp: MP[A])(predicate: A => Boolean): MP[A] = {
    filter(mp)(!predicate(_))
  }

  def takeUntil[A](mp: MP[A])(predicate: A => Boolean): MP[List[A]] = {
    many(filterNot(mp)(predicate))
  }

  def takeWhile[A](mp: MP[A])(predicate: A => Boolean): MP[List[A]] = {
    many(filter(mp)(predicate))
  }
}
