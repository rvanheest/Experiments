package monadics.laws

import monadics.structures.Applicative

import scala.language.higherKinds

trait ApplicativeLaws[App[_]] extends FunctorLaws[App] {

	implicit val instance: Applicative[App]

	// pure id <*> v == v
	def applicativeIdentity[A](appA: App[A]): IsEquals[App[A]] = {
		instance.<*>(instance.create[A => A](Predef.identity), appA) === appA
	}

	// pure f <*> pure x == pure (f x)
	def applicativeHomomorphism[A, B](a: A, f: A => B): IsEquals[App[B]] = {
		instance.<*>(instance.create(f), instance.create(a)) === instance.create(f(a))
	}

	// u <*> pure y == pure ($ y) <*> u
	def applicativeInterchange[A, B](a: A, appF: App[A => B]): IsEquals[App[B]] = {
		instance.<*>(appF, instance.create(a)) === instance.<*>(instance.create[(A => B) => B](_(a)), appF)
	}

	// fmap v f == pure ($ f) <*> v
	def applicativeMap[A, B](appA: App[A], f: A => B): IsEquals[App[B]] = {
		instance.map(appA)(f) === instance.<*>(instance.create(f), appA)
	}

	// u <*> (v <*> w) == ((pure (.) <*> u) <*> v) <*> w
	def applicativeComposition[A, B, C](appAToB: App[A => B], appCToA: App[C => A], appC: App[C]): IsEquals[App[B]] = {
		val left = instance.<*>(appAToB, instance.<*>(appCToA, appC))

		val pure = instance.create[(A => B) => (C => A) => C => B](_.compose)
		val f = instance.<*>(pure, appAToB)
		val g = instance.<*>(f, appCToA)
		val right = instance.<*>(g, appC)

		left === right
	}
}

object ApplicativeLaws {
	def apply[App[_]](implicit app: Applicative[App]): ApplicativeLaws[App] = new ApplicativeLaws[App] {
		val instance: Applicative[App] = app
	}
}
