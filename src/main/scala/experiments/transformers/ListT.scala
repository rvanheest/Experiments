package experiments.transformers

import experiments.monadics.MonadPlus

case class ListT[M[_], A](run: M[List[A]])(implicit m: MonadPlus[M]) {
	import ListT.lift

	def map[B](f: A => B): ListT[M, B] = {
		lift(m.map(run)(_ map f))
	}

	def flatMap[B](f: A => ListT[M, B]): ListT[M, B] = {
		val res = m.flatMap(run) {
			case Nil => m.create(List[B]())
			case list => list.map(f.andThen(_.run)).fold(m.empty)((a, b) => m.flatMap(a)(bs1 => m.map(b)(bs1 ++ _)))
		}

		lift(res)
	}

	def ++(that: ListT[M, A]) = {
		lift(m.flatMap(run)(list1 => m.map(that.run)(list1 ++ _)))
	}

	def filter(predicate: A => Boolean): ListT[M, A] = {
		lift(m.map(run)(_.filter(predicate)))
	}
}
object ListT {
	def lift[M[_], A](x: M[List[A]])(implicit m: MonadPlus[M]): ListT[M, A] = new ListT(x)
}
