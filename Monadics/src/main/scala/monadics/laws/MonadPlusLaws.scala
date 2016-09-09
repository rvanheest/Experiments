package monadics.laws

import monadics.structures.MonadPlus

import scala.language.higherKinds

trait MonadPlusLaws[MP[_]] {

	implicit def monadPlus: MonadPlus[MP]

	// empty <|> x == x
	def monadPlusLeftEmpty[A](mpA: MP[A]) = {
		monadPlus.orElse(monadPlus.empty, mpA) == mpA
	}

	// x <|> empty == x
	def monadPlusRightEmpty[A](mpA: MP[A]) = {
		monadPlus.orElse(mpA, monadPlus.empty) == mpA
	}

	// (x <|> y) <|> z == x <|> (y <|> z)
	def monadPlusAssociativity[A, B >: A, C >: B](mpA: MP[A], mpB: MP[B], mpC: MP[C]) = {
		monadPlus.orElse(monadPlus.orElse(mpA, mpB), mpC) == monadPlus.orElse(mpA, monadPlus.orElse(mpB, mpC))
	}

	// mzero >>= f == mzero
	def monadPlusEmptyFlatMap[A, B](f: A => MP[B]) = {
		monadPlus.flatMap(monadPlus.empty)(f) == monadPlus.empty[B]
	}

	// v >> mzero == mzero
	def monadPlusAndThenEmpty[A](mpA: MP[A]) = {
		monadPlus.andThen(mpA, monadPlus.empty) == monadPlus.empty
	}
}

object MonadPlusLaws {
	def apply[MP[_]](implicit mp: MonadPlus[MP]): MonadPlusLaws[MP] = new MonadPlusLaws[MP] {
		def monadPlus: MonadPlus[MP] = mp
	}
}
