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

  implicit def tryOfSemigroupIsMonoid[A]: Semigroup[Try[A]] = new Semigroup[Try[A]] {
    override def combine(ta: Try[A], tb: => Try[A]): Try[A] = ta.orElse(tb)
  }

  implicit val tryIsMonadPlusAndMonadFail = new MonadFail[Try] with Traverse[Try] {
    def create[A](a: A): Try[A] = Try(a)

    def fail[A](e: Throwable): Try[A] = Failure(e)

    override def map[A, B](functor: Try[A])(f: A => B): Try[B] = functor.map(f)

    def flatMap[A, B](monad: Try[A])(f: A => Try[B]): Try[B] = monad.flatMap(f)

    def traverse[G[_], A, B](fa: Try[A])(f: A => G[B])(implicit applicative: Applicative[G]): G[Try[B]] = {
      fa match {
        case Success(a) => applicative.map(f(a))(Try(_))
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

  implicit class TryMonadOperators[A](val t: Try[A])(implicit monadTraverse: Monad[Try] with Traverse[Try]) {
    def as[B](b: => B): Try[B] = monadTraverse.as(t, b)

    def void: Try[Unit] = monadTraverse.void(t)

    def zipWith[B](f: A => B): Try[(A, B)] = monadTraverse.zipWith(t)(f)

    def <*>[B, C](other: Try[B])(implicit ev: Try[A] <:< Try[(B => C)]): Try[C] = monadTraverse.<*>(t, other)

    def traverse[G[_], B](f: A => G[B])(implicit applicative: Applicative[G]): G[Try[B]] = monadTraverse.traverse(t)(f)

    def sequence[G[_], B](implicit ev: Try[A] <:< Try[G[B]], applicative: Applicative[G]): G[Try[B]] = {
      monadTraverse.sequence(t)
    }
  }
}

object tryMonad extends tryMonad
