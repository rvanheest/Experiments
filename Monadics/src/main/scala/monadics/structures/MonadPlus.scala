package monadics.structures

import scala.language.higherKinds

trait MonadPlus[MP[_]] extends Monad[MP] with Alternative[MP] {
	def mplus[A, B >: A](mp1: MP[A], mp2: MP[B]): MP[B] = {
		orElse(mp1, mp2)
	}

	def filter[A](mp: MP[A])(predicate: A => Boolean): MP[A] = {
		flatMap(mp)(a => if (predicate(a)) create(a) else empty[A])
	}

	def filterNot[A](mp: MP[A])(predicate: A => Boolean): MP[A] = {
		filter(mp)(!predicate(_))
	}

	def takeUntil[A](mp: MP[A])(predicate: A => Boolean): MP[List[A]] = {
		many(filterNot(mp)(predicate))
	}

	def takeWhile[A](mp: MP[A])(predicate: A => Boolean): MP[List[A]] = {
		many(filter(mp)(predicate))
	}
}