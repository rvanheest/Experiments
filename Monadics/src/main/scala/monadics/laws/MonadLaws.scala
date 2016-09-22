package monadics.laws

import monadics.structures.Monad

import scala.language.higherKinds

trait MonadLaws[M[_]] extends ApplicativeLaws[M] {

	implicit val instance: Monad[M]

	// return a >>= k == k a
	def monadLeftIdentity[A, B](a: A, f: A => M[B]) = {
		instance.flatMap(instance.create(a))(f) == f(a)
	}

	// m >>= return == m
	def monadRightIdentity[A](ma: M[A]) = {
		instance.flatMap(ma)(a => instance.create(a)) == ma
	}

	// m >>= (\x -> k x >>= h) == (m >>= k) >>= h
	def monadAssociativity[A, B, C](ma: M[A], f: A => M[B], g: B => M[C]) = {
		instance.flatMap(instance.flatMap(ma)(f))(g) == instance.flatMap(ma)(a => instance.flatMap(f(a))(b => g(b)))
	}
}

object MonadLaws {
	def apply[M[_]](implicit m: Monad[M]): MonadLaws[M] = new MonadLaws[M] {
		val instance: Monad[M] = m
	}
}
