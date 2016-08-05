package monadics.structures

import scala.language.higherKinds

trait Monad[M[_]] extends Applicative[M] {
	def fail[A](s: String): M[A] = fail(new Exception(s))

	def fail[A](e: Throwable): M[A]

	override def <*>[A, B](appFunc: M[(A) => B], appA: M[A]): M[B] = {
		flatMap(appA)(a => map(appFunc)(_(a)))
	}

	override def *>[A, B](appA: M[A], appB: M[B]): M[B] = {
		andThen(appA, appB)
	}

	override def <*[A, B](appA: M[A], appB: M[B]): M[A] = {
		thenAnd(appA, appB)
	}

	def flatMap[A, B](monad: M[A])(f: A => M[B]): M[B]

	def andThen[A, B](monadA: M[A], monadB: M[B]): M[B] = {
		flatMap(monadA)(_ => monadB)
	}

	def thenAnd[A, B](monadA: M[A], monadB: M[B]): M[A] = {
		flatMap(monadA)(a => map(monadB)(_ => a))
	}

	def flatten[A, B](monadA: M[A])(implicit ev: A <:< M[B]): M[B] = {
		flatMap(monadA)(ev)
	}
}
