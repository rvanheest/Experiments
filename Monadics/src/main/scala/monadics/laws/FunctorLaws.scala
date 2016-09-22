package monadics.laws

import monadics.structures.Functor

import scala.language.higherKinds

trait FunctorLaws[F[_]] {

	implicit val instance: Functor[F]

	// fmap id == id
	def identity[A](fa: F[A]) = {
		instance.map(fa)(a => a) == fa
	}

	// fmap (g . h) == (fmap g) . (fmap h)
	def composition[A, B, C](fa: F[A], f: A => B, g: B => C) = {
		instance.map(fa)(g compose f) == instance.map(instance.map(fa)(f))(g)
	}
}

object FunctorLaws {
	def apply[F[_]](implicit f: Functor[F]): FunctorLaws[F] = new FunctorLaws[F] {
		val instance: Functor[F] = f
	}
}
