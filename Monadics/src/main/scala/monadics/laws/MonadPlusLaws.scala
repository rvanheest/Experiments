package monadics.laws

import monadics.structures.MonadPlus

import scala.language.higherKinds

trait MonadPlusLaws[MP[_]] extends MonadLaws[MP] {

	implicit val instance: MonadPlus[MP]

	// empty <|> x == x
	def monadPlusLeftEmpty[A](mpA: MP[A]): Boolean = {
		instance.orElse(instance.empty, mpA) == mpA
	}

	// x <|> empty == x
	def monadPlusRightEmpty[A](mpA: MP[A]): Boolean = {
		instance.orElse(mpA, instance.empty) == mpA
	}

	// (x <|> y) <|> z == x <|> (y <|> z)
	def monadPlusAssociativity[A, B >: A, C >: B](mpA: MP[A], mpB: MP[B], mpC: MP[C]): Boolean = {
		instance.orElse(instance.orElse(mpA, mpB), mpC) == instance.orElse(mpA, instance.orElse(mpB, mpC))
	}

	// mzero >>= f == mzero
	def monadPlusEmptyFlatMap[A, B](f: A => MP[B]): Boolean = {
		instance.flatMap(instance.empty)(f) == instance.empty[B]
	}

	// v >> mzero == mzero
	def monadPlusAndThenEmpty[A](mpA: MP[A]): Boolean = {
		instance.andThen(mpA, instance.empty) == instance.empty
	}
}

object MonadPlusLaws {
	def apply[MP[_]](implicit mp: MonadPlus[MP]): MonadPlusLaws[MP] = new MonadPlusLaws[MP] {
		val instance: MonadPlus[MP] = mp
	}
}
