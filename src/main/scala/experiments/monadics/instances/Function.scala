package experiments.monadics.instances

import experiments.monadics.{Category, Functor}

case class Function[A, B](f: A => B)(implicit cat: Category[Function], func: Functor[({ type s[x] = Function[A, x] })#s]) {

	def apply(a: A): B = f(a)

	def andThen[C](g: Function[B, C]): Function[A, C] = cat.compose(g, this)

	def compose[C](g: Function[C, A]): Function[C, B] = cat.compose(this, g)

	def map[C](g: B => C): Function[A, C] = func.map(this)(g)
}
object Function {
	def identity[A](implicit cat: Category[Function]) = cat.id[A]
}
