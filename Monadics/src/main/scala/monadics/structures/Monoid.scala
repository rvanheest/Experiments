package monadics.structures

trait Monoid[M] extends Semigroup[M] {
	def empty: M
}

object Monoid {

	def create[M](default: => M)(comb: (M, => M) => M): Monoid[M] = new Monoid[M] {
		override def empty: M = default
		override def combine(a1: M, a2: => M): M = comb(a1, a2)
	}
}
