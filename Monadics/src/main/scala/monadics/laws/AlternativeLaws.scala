package monadics.laws

import monadics.structures.Alternative

import scala.language.higherKinds

trait AlternativeLaws[Alt[_]] extends ApplicativeLaws[Alt] {

	implicit val instance: Alternative[Alt]

	// empty <|> x == x
	def alternativeLeftEmpty[A](altA: Alt[A]): Boolean = {
		instance.orElse(instance.empty, altA) == altA
	}

	// x <|> empty == x
	def alternativeRightEmpty[A](altA: Alt[A]): Boolean = {
		instance.orElse(altA, instance.empty) == altA
	}

	// (x <|> y) <|> z == x <|> (y <|> z)
	def alternativeAssociativity[A, B >: A, C >: B](altA: Alt[A], altB: Alt[B], altC: Alt[C]): Boolean = {
		instance.orElse(instance.orElse(altA, altB), altC) == instance.orElse(altA, instance.orElse(altB, altC))
	}
}

object AlternativeLaws {
	def apply[Alt[_]](implicit alt: Alternative[Alt]): AlternativeLaws[Alt] = new AlternativeLaws[Alt] {
		val instance: Alternative[Alt] = alt
	}
}
