package monadics.laws

import monadics.structures.MonadPlus

import scala.language.higherKinds

trait MonadPlusLaws[MP[_]] extends MonadLaws[MP] {

	implicit val instance: MonadPlus[MP]

	// empty <|> x == x
	def monadPlusLeftEmpty[A](mpA: MP[A]): IsEquals[MP[A]] = {
		instance.combine(instance.empty[A], mpA) === mpA
	}

	// x <|> empty == x
	def monadPlusRightEmpty[A](mpA: MP[A]): IsEquals[MP[A]] = {
		instance.combine(mpA, instance.empty[A]) === mpA
	}

	// (x <|> y) <|> z == x <|> (y <|> z)
	def monadPlusAssociativity[A, B >: A, C >: B](mpA: MP[A], mpB: MP[B], mpC: MP[C]): IsEquals[MP[C]] = {
		instance.combine(instance.combine(mpA, mpB), mpC) === instance.combine(mpA, instance.combine(mpB, mpC))
	}

	// mzero >>= f == mzero
	def monadPlusEmptyFlatMap[A, B](f: A => MP[B]): IsEquals[MP[B]] = {
		instance.flatMap(instance.empty[A])(f) === instance.empty[B]
	}

	// v >> mzero == mzero
	def monadPlusAndThenEmpty[A](mpA: MP[A]): IsEquals[MP[A]] = {
		instance.andThen(mpA, instance.empty[A]) === instance.empty[A]
	}
}

object MonadPlusLaws {
	def apply[MP[_]](implicit mp: MonadPlus[MP]): MonadPlusLaws[MP] = new MonadPlusLaws[MP] {
		val instance: MonadPlus[MP] = mp
	}
}
