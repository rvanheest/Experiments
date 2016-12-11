package monadics.laws

import monadics.structures.MonadPlus

import scala.language.higherKinds

trait MonadPlusLaws[MP[_]] extends MonadFilterLaws[MP] with AlternativeLaws[MP] {

	implicit val instance: MonadPlus[MP]

	def monadCombineLeftDistributivity[A, B](mpX: MP[A], mpY: MP[A], f: A => MP[B]): IsEquals[MP[B]] = {
		instance.flatMap(instance.combine(mpX, mpY))(f) === instance.combine(instance.flatMap(mpX)(f), instance.flatMap(mpY)(f))
	}
}

object MonadPlusLaws {
	def apply[MP[_]](implicit mp: MonadPlus[MP]): MonadPlusLaws[MP] = new MonadPlusLaws[MP] {
		val instance: MonadPlus[MP] = mp
	}
}
