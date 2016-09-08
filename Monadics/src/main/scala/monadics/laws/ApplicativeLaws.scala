package monadics.laws

import monadics.structures.{Applicative, Functor}

import scala.language.higherKinds

trait ApplicativeLaws[App[_]] {

	implicit def applicative: Applicative[App]

	def applicativeIdentity[A](appA: App[A]) = {
		applicative.<*>(applicative.create[A => A](a => a), appA) == appA
	}

	def applicativeHomomorphism[A, B](a: A, f: A => B) = {
		applicative.<*>(applicative.create(f), applicative.create(a)) == applicative.create(f(a))
	}

	def applicativeInterchange[A, B](a: A, appF: App[A => B]) = {
		applicative.<*>(appF, applicative.create(a)) == applicative.<*>(applicative.create[(A => B) => B](f => f(a)), appF)
	}

	def applicativeMap[A, B](appA: App[A], f: A => B) = {
		applicative.map(appA)(f) == applicative.<*>(applicative.create(f), appA)
	}
}

object ApplicativeLaws {
	def apply[App[_]](implicit app: Applicative[App]): ApplicativeLaws[App] = new ApplicativeLaws[App] {
		def applicative: Applicative[App] = app
	}
}
