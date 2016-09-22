package monadics.structures

import scala.language.higherKinds

trait Functor[F[_]] {
	def map[A, B](functor: F[A])(f: A => B): F[B]

	def as[A, B](functor: F[A], b: => B): F[B] = {
		map(functor)(_ => b)
	}

	def void[A](functor: F[A]): F[Unit] = {
		as(functor, ())
	}
}
