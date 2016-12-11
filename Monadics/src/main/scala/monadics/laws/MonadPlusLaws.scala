package monadics.laws

import monadics.structures.MonadPlus

import scala.language.higherKinds

trait MonadPlusLaws[MP[_]] extends MonadFilterLaws[MP] with AlternativeLaws[MP] {

	implicit val instance: MonadPlus[MP]

	def monadplusCombineLeftDistributivity[A, B](mpX: MP[A], mpY: MP[A], f: A => MP[B]): IsEquals[MP[B]] = {
		instance.flatMap(instance.combine(mpX, mpY))(f) === instance.combine(instance.flatMap(mpX)(f), instance.flatMap(mpY)(f))
	}

	def monadplusLeftCatch[A](a: A, mpA: MP[A]): IsEquals[MP[A]] = {
		instance.combine(instance.create(a), mpA) === instance.create(a)
	}
}

object MonadPlusLaws {
	def apply[MP[_]](implicit mp: MonadPlus[MP]): MonadPlusLaws[MP] = new MonadPlusLaws[MP] {
		val instance: MonadPlus[MP] = mp
	}
}
