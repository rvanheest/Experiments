package monadics.structures

import scala.language.higherKinds

trait MonadPlus[MP[_]] extends MonadFilter[MP] with Alternative[MP] {
	def takeUntil[A](mp: MP[A])(predicate: A => Boolean): MP[List[A]] = {
		many(filterNot(mp)(predicate))
	}

	def takeWhile[A](mp: MP[A])(predicate: A => Boolean): MP[List[A]] = {
		many(filter(mp)(predicate))
	}
}
