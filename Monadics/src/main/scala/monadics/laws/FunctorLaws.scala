package monadics.laws

import monadics.structures.Functor

import scala.language.higherKinds

trait FunctorLaws[F[_]] {

	implicit def functor: Functor[F]

	def identity[A](fa: F[A]) = {
		functor.map(fa)(a => a) == fa
	}

	def composition[A, B, C](fa: F[A], f: A => B, g: B => C) = {
		functor.map(functor.map(fa)(f))(g) == functor.map(fa)(g compose f)
	}
}

object FunctorLaws {
	def apply[F[_]](implicit f: Functor[F]): FunctorLaws[F] = new FunctorLaws[F] {
		def functor: Functor[F] = f
	}
}
