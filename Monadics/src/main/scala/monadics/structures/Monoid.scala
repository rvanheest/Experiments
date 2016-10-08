package monadics.structures

trait Monoid[M] extends Semigroup[M] {
	def empty: M
}
