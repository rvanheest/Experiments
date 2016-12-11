package monadics.laws

import monadics.structures.{Alternative, MonoidK}

import scala.language.higherKinds

trait AlternativeLaws[Alt[_]] extends ApplicativeLaws[Alt] with MonoidKLaws[Alt] {

	implicit val instance: Alternative[Alt] with MonoidK[Alt]

	// empty <|> x == x
	def alternativeLeftEmpty[A](altA: Alt[A]): IsEquals[Alt[A]] = {
		instance.combine(instance.empty[A], altA) === altA
	}

	// x <|> empty == x
	def alternativeRightEmpty[A](altA: Alt[A]): IsEquals[Alt[A]] = {
		instance.combine(altA, instance.empty[A]) === altA
	}

	// (x <|> y) <|> z == x <|> (y <|> z)
	def alternativeAssociativity[A, B >: A, C >: B](altA: Alt[A], altB: Alt[B], altC: Alt[C]): IsEquals[Alt[C]] = {
		instance.combine(instance.combine(altA, altB), altC) === instance.combine(altA, instance.combine(altB, altC))
	}
}

object AlternativeLaws {
	def apply[Alt[_]](implicit alt: Alternative[Alt] with MonoidK[Alt]): AlternativeLaws[Alt] = new AlternativeLaws[Alt] {
		val instance: Alternative[Alt] = alt
	}
}
