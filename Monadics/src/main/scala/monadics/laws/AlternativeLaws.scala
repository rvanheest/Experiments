package monadics.laws

import monadics.structures.Alternative

import scala.language.higherKinds

trait AlternativeLaws[Alt[_]] extends ApplicativeLaws[Alt] {

	implicit val instance: Alternative[Alt]

	// empty <|> x == x
	def alternativeLeftEmpty[A](altA: Alt[A]): IsEquals[Alt[A]] = {
		instance.orElse(instance.empty[A], altA) === altA
	}

	// x <|> empty == x
	def alternativeRightEmpty[A](altA: Alt[A]): IsEquals[Alt[A]] = {
		instance.orElse(altA, instance.empty[A]) === altA
	}

	// (x <|> y) <|> z == x <|> (y <|> z)
	def alternativeAssociativity[A, B >: A, C >: B](altA: Alt[A], altB: Alt[B], altC: Alt[C]): IsEquals[Alt[C]] = {
		instance.orElse(instance.orElse(altA, altB), altC) === instance.orElse(altA, instance.orElse(altB, altC))
	}
}

object AlternativeLaws {
	def apply[Alt[_]](implicit alt: Alternative[Alt]): AlternativeLaws[Alt] = new AlternativeLaws[Alt] {
		val instance: Alternative[Alt] = alt
	}
}
