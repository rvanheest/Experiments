package monadics.structures

import scala.language.higherKinds

trait Functor[F[_]] {
	def map[A, B](functor: F[A])(f: A => B): F[B]
}
