package monadics.structures

trait Semigroup[S] {
	def append(a1: S, a2: => S): S
}
