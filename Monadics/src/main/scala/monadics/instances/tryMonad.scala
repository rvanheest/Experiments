package monadics.instances

import monadics.structures.{MonadFail, MonadPlus, Monoid, Semigroup}

import scala.util.{Failure, Try}

trait tryMonad {

  implicit def tryOfSemigroupIsMonoid[A](implicit semigroup: Semigroup[A]): Semigroup[Try[A]] = new Monoid[Try[A]] {
    override def empty: Try[A] = Failure(new NoSuchElementException("empty"))

    override def combine(ta: Try[A], tb: => Try[A]): Try[A] = {
      ta.map(a =>
        tb.map(b => Try(semigroup.combine(a, b)))
          .getOrElse(ta))
        .getOrElse(tb)
    }
  }

  implicit val tryIsMonadPlusAndMonadFail = new MonadPlus[Try] with MonadFail[Try] {
    def empty[A]: Try[A] = Failure(new NoSuchElementException("empty"))

    def create[A](a: A): Try[A] = Try(a)

    def fail[A](e: Throwable): Try[A] = Failure(e)

    override def map[A, B](functor: Try[A])(f: A => B): Try[B] = {
      functor.map(f)
    }

    def flatMap[A, B](monad: Try[A])(f: A => Try[B]): Try[B] = {
      monad.flatMap(f)
    }

    def orElse[A, B >: A](try1: Try[A], try2: => Try[B]): Try[B] = {
      try1.orElse(try2)
    }
  }

  implicit class TryMonadPlusOperators[A](val t: Try[A])(implicit monadPlus: MonadPlus[Try]) {
    def as[B](b: => B): Try[B] = monadPlus.as(t, b)

    def void: Try[Unit] = monadPlus.void(t)

    def zipWith[B](f: A => B): Try[(A, B)] = monadPlus.zipWith(t)(f)

    def <*>[B, C](other: Try[B])(implicit ev: A <:< (B => C)): Try[C] = monadPlus.<*>(t.map(ev), other)
  }
}

object tryMonad extends tryMonad
