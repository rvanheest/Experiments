package monadics.laws

import monadics.structures.Functor

import scala.language.higherKinds

trait FunctorLaws[F[_]] {

	implicit def functor: Functor[F]

	// fmap id == id
	def identity[A](fa: F[A]) = {
		functor.map(fa)(a => a) == fa
	}

	// fmap (g . h) == (fmap g) . (fmap h)
	def composition[A, B, C](fa: F[A], f: A => B, g: B => C) = {
		functor.map(fa)(g compose f) == functor.map(functor.map(fa)(f))(g)
	}
}

object FunctorLaws {
	def apply[F[_]](implicit f: Functor[F]): FunctorLaws[F] = new FunctorLaws[F] {
		def functor: Functor[F] = f
	}
}
