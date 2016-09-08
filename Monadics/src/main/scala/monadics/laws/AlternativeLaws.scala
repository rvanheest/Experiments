package monadics.laws

import monadics.structures.Alternative

import scala.language.higherKinds

trait AlternativeLaws[Alt[_]] {

	implicit def alternative: Alternative[Alt]

	// empty <|> x == x
	def alternativeLeftEmpty[A](altA: Alt[A]) = {
		alternative.orElse(alternative.empty, altA) == altA
	}

	// x <|> empty == x
	def alternativeRightEmpty[A](altA: Alt[A]) = {
		alternative.orElse(altA, alternative.empty) == altA
	}

	// (x <|> y) <|> z == x <|> (y <|> z)
	def alternativeAssociativity[A, B >: A, C >: B](altA: Alt[A], altB: Alt[B], altC: Alt[C]) = {
		alternative.orElse(alternative.orElse(altA, altB), altC) == alternative.orElse(altA, alternative.orElse(altB, altC))
	}
}

object AlternativeLaws {
	def apply[Alt[_]](implicit alt: Alternative[Alt]): AlternativeLaws[Alt] = new AlternativeLaws[Alt] {
		def alternative: Alternative[Alt] = alt
	}
}
