package monadics.instances

import monadics.structures._

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

trait tryMonad {

  implicit def tryIsEquals[A](implicit aEquals: Equals[A]): Equals[Try[A]] = {
    Equals.create {
      case (Success(x), Success(y)) => aEquals.equals(x, y)
      case (Failure(ex1), Failure(ex2)) => ex1.getClass == ex2.getClass && ex1.getMessage == ex2.getMessage
      case _ => false
    }
  }

  implicit def tryOfSemigroupIsMonoid[A](implicit semigroup: Semigroup[A]): Semigroup[Try[A]] = new Monoid[Try[A]] {
    override def empty: Try[A] = Failure(new NoSuchElementException("empty"))

    override def combine(ta: Try[A], tb: => Try[A]): Try[A] = {
      ta.map(a =>
        tb.map(b => Try(semigroup.combine(a, b)))
          .getOrElse(ta))
        .getOrElse(tb)
    }
  }

  implicit val tryIsMonadPlusAndMonadFail = new MonadFail[Try] with Traverse[Try] {
    def create[A](a: A): Try[A] = Try(a)

    def fail[A](e: Throwable): Try[A] = Failure(e)

    override def map[A, B](functor: Try[A])(f: A => B): Try[B] = {
      functor.map(f)
    }

    def flatMap[A, B](monad: Try[A])(f: A => Try[B]): Try[B] = {
      monad.flatMap(f)
    }

    def traverse[G[_], A, B](fa: Try[A])(f: A => G[B])(implicit applicative: Applicative[G]): G[Try[B]] = {
      fa match {
        case Success(a) => applicative.map(f(a))(Success(_))
        case Failure(e) => applicative.create(Failure(e))
      }
    }

    override def sequence[G[_], A](fa: Try[G[A]])(implicit applicative: Applicative[G]): G[Try[A]] = {
      fa match {
        case Success(ga) => applicative.map(ga)(Try(_))
        case Failure(e) => applicative.create(Failure(e))
      }
    }
  }

  implicit class TryMonadOperators[A](val t: Try[A])(implicit monad: Monad[Try]) {
    def as[B](b: => B): Try[B] = monad.as(t, b)

    def void: Try[Unit] = monad.void(t)

    def zipWith[B](f: A => B): Try[(A, B)] = monad.zipWith(t)(f)

    def <*>[B, C](other: Try[B])(implicit ev: Try[A] <:< Try[(B => C)]): Try[C] = monad.<*>(t, other)
  }
}

object tryMonad extends tryMonad
