package monadics.structures

trait Semigroup[S] {
	def combine(a1: S, a2: => S): S
}

object Semigroup {

	def create[S](comb: (S, => S) => S): Semigroup[S] = new Semigroup[S] {
		override def combine(a1: S, a2: => S): S = comb.apply(a1, a2)
	}
}
