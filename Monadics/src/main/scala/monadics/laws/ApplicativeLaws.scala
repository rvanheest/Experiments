package monadics.laws

import monadics.structures.Applicative

import scala.language.higherKinds

trait ApplicativeLaws[App[_]] {

	implicit def applicative: Applicative[App]

	// pure id <*> v == v
	def applicativeIdentity[A](appA: App[A]) = {
		applicative.<*>(applicative.create[A => A](a => a), appA) == appA
	}

	// pure f <*> pure x == pure (f x)
	def applicativeHomomorphism[A, B](a: A, f: A => B) = {
		applicative.<*>(applicative.create(f), applicative.create(a)) == applicative.create(f(a))
	}

	// u <*> pure y == pure ($ y) <*> u
	def applicativeInterchange[A, B](a: A, appF: App[A => B]) = {
		applicative.<*>(appF, applicative.create(a)) == applicative.<*>(applicative.create[(A => B) => B](f => f(a)), appF)
	}

	// fmap v f == pure ($ f) <*> v
	def applicativeMap[A, B](appA: App[A], f: A => B) = {
		applicative.map(appA)(f) == applicative.<*>(applicative.create(f), appA)
	}

	// TODO u <*> (v <*> w) == pure (.) <*> u <*> v <*> w
}

object ApplicativeLaws {
	def apply[App[_]](implicit app: Applicative[App]): ApplicativeLaws[App] = new ApplicativeLaws[App] {
		def applicative: Applicative[App] = app
	}
}
