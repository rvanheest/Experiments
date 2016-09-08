package monadics.laws

import monadics.structures.Monad

import scala.language.higherKinds

trait MonadLaws[M[_]] {

	implicit def monad: Monad[M]

	def monadLeftIdentity[A, B](a: A, f: A => M[B]) = {
		monad.flatMap(monad.create(a))(f) == f(a)
	}

	def monadRightIdentity[A](ma: M[A]) = {
		monad.flatMap(ma)(a => monad.create(a)) == ma
	}

	def monadAssociativity[A, B, C](ma: M[A], f: A => M[B], g: B => M[C]) = {
		monad.flatMap(monad.flatMap(ma)(f))(g) == monad.flatMap(ma)(a => monad.flatMap(f(a))(b => g(b)))
	}
}

object MonadLaws {
	def apply[M[_]](implicit m: Monad[M]): MonadLaws[M] = new MonadLaws[M] {
		def monad: Monad[M] = m
	}
}
