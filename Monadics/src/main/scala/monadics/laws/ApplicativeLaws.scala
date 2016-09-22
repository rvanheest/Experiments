package monadics.laws

import monadics.structures.Applicative

import scala.language.higherKinds

trait ApplicativeLaws[App[_]] extends FunctorLaws[App] {

	implicit val instance: Applicative[App]

	// pure id <*> v == v
	def applicativeIdentity[A](appA: App[A]) = {
		instance.<*>(instance.create[A => A](a => a), appA) == appA
	}

	// pure f <*> pure x == pure (f x)
	def applicativeHomomorphism[A, B](a: A, f: A => B) = {
		instance.<*>(instance.create(f), instance.create(a)) == instance.create(f(a))
	}

	// u <*> pure y == pure ($ y) <*> u
	def applicativeInterchange[A, B](a: A, appF: App[A => B]) = {
		instance.<*>(appF, instance.create(a)) == instance.<*>(instance.create[(A => B) => B](f => f(a)), appF)
	}

	// fmap v f == pure ($ f) <*> v
	def applicativeMap[A, B](appA: App[A], f: A => B) = {
		instance.map(appA)(f) == instance.<*>(instance.create(f), appA)
	}

	// TODO u <*> (v <*> w) == pure (.) <*> u <*> v <*> w
}

object ApplicativeLaws {
	def apply[App[_]](implicit app: Applicative[App]): ApplicativeLaws[App] = new ApplicativeLaws[App] {
		val instance: Applicative[App] = app
	}
}
