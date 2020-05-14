class Foo(x: Int) {
	def add(y: Int): Int = x + y
}

// forall a T[a] is true
trait Forall {
	def apply[A](a: A): Option[A]
}

// there exists an a for which T[a] is true
trait Exists {
	type A
	def apply(a: A): Option[A]
}

trait Thing[-A, +B] {
	type X

	def source(a: A): X
	def sink(x: X): B

	def apply(x: X): X
}

def bippy(t: Thing[Int, String]) = {
	val state: t.X = t.source(42)
	val state2: t.X = t(state)

	t.sink(state2)
}
