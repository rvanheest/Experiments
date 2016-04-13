package experiments.monadics

import scala.language.higherKinds

trait Monoid[M[_]] {
  def mappend[T, S >: T](a: M[T], b: M[S]): M[S]
}

trait Functor[F[_]] {
  def map[A, B](f: A => B, functor: F[A]): F[B]
}

trait Applicative[App[_]] extends Functor[App] {
  implicit def apply[A](a: A): App[A]

  def <*>[A, B](appFunc: App[A => B], appA: App[A]): App[B]

  def *>[A, B](appA: App[A], appB: App[B]): App[B] = {
    <*>(map[A, B => B](_ => identity[B], appA), appB)
  }

  def <*[A, B](appA: App[A], appB: App[B]): App[A] = {
    liftA2[A, B, A](a => b => a, appA, appB)
  }

  def <**>[A, B](appA: App[A], appFunc: App[A => B]): App[B] = {
    liftA2[A, A => B, B](a => f => f(a), appA, appFunc)
  }

  def liftA[A, B](f: A => B, appA: App[A]): App[B] = {
    <*>(apply(f), appA)
  }

  def liftA2[A, B, C](f: A => B => C, appA: App[A], appB: App[B]): App[C] = {
    <*>(map(f, appA), appB)
  }

  def liftA3[A, B, C, D](f: A => B => C => D, appA: App[A], appB: App[B], appC: App[C]): App[D] = {
    <*>(liftA2(f, appA, appB), appC)
  }
}

trait Monad[M[_]] extends Applicative[M] {
  override def <*>[A, B](mFunc: M[A => B], mA: M[A]): M[B] = {
    liftM2[A => B, A, B](f => f(_), mFunc, mA)
  }

  override def *>[A, B](mA: M[A], mB: M[B]): M[B] = {
    >>(mA, mB)
  }

  override def <*[A, B](mA: M[A], mB: M[B]): M[A] = {
    <<(mA, mB)
  }

  def >>=[A, B](monad: M[A], f: A => M[B]): M[B]

  def >>[A, B](mA: M[A], mB: M[B]): M[B] = {
    >>=[A, B](mA, a => mB)
  }

  def <<[A, B](mA: M[A], mB: M[B]): M[A] = {
    >>=[A, A](mA, a => >>=[B, A](mB, _ => apply(a)))
  }

  def liftM[A, B](f: A => B, mA: M[A]): M[B] = {
    map[A, B](f(_), mA)
  }

  def liftM2[A, B, C](f: A => B => C, mA: M[A], mB: M[B]): M[C] = {
    >>=[A, C](mA, a => map[B, C](f(a)(_), mB))
  }

  def liftM3[A, B, C, D](f: A => B => C => D, mA: M[A], mB: M[B], mC: M[C]): M[D] = {
    >>=[A, D](mA, a => >>=[B, D](mB, b => map[C, D](f(a)(b)(_), mC)))
  }
}

trait Alternative[Alt[_]] extends Applicative[Alt] {
  implicit def empty[A]: Alt[A]

  def <|>[A](alt1: Alt[A], alt2: Alt[A]): Alt[A]

  def some[A](alt: Alt[A]): Alt[List[A]] = some_v(alt)

  def many[A](alt: Alt[A]): Alt[List[A]] = many_v(alt)

  private def many_v[A](alt: Alt[A]): Alt[List[A]] = <|>(some_v(alt), apply(Nil))

  private def some_v[A](alt: Alt[A]): Alt[List[A]] = {
    <*>(map[A, List[A] => List[A]](a => a :: _, alt), many_v(alt))
  }
}

trait MonadPlus[MP[_]] extends Monad[MP] with Alternative[MP] {
  def mplus[A](mp1: MP[A], mp2: MP[A]): MP[A] = {
    <|>[A](mp1, mp2)
  }
}
