package monadics.laws

import monadics.structures.{Alternative, MonoidK}

import scala.language.higherKinds

trait AlternativeLaws[Alt[_]] extends ApplicativeLaws[Alt] with MonoidKLaws[Alt] {

	implicit val instance: Alternative[Alt] with MonoidK[Alt]

	def alternativeRightDistributivity[A, B](altA: Alt[A], altF: Alt[A => B], altG: Alt[A => B]): IsEquals[Alt[B]] = {
		instance.<*>(instance.combine(altF, altG), altA) === instance.combine(instance.<*>(altF, altA), instance.<*>(altG, altA))
	}

	def alternativeRightAbsorption[A, B](altF: Alt[A => B]): IsEquals[Alt[B]] = {
		instance.<*>(altF, instance.empty[A]) === instance.empty[B]
	}

	def alternativeLeftDistributivity[A, B](altX: Alt[A], altY: Alt[A], f: A => B): IsEquals[Alt[B]] = {
		instance.map(instance.combine(altX, altY))(f) === instance.combine(instance.map(altX)(f), instance.map(altY)(f))
	}
}

object AlternativeLaws {
	def apply[Alt[_]](implicit alt: Alternative[Alt] with MonoidK[Alt]): AlternativeLaws[Alt] = new AlternativeLaws[Alt] {
		val instance: Alternative[Alt] = alt
	}
}
