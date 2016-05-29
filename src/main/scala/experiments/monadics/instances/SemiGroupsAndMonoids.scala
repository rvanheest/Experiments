package experiments.monadics.instances

import experiments.monadics.Monoid

case class Sum[A: Numeric](sum: A)(implicit s: Monoid[Sum[A]]) {

	def +(s2: Sum[A]): Sum[A] = s.append(this, s2)
}
object Sum {
	def empty[A: Numeric](implicit s: Monoid[Sum[A]]): Sum[A] = s.empty
}

case class Product[A: Numeric](product: A)(implicit s: Monoid[Product[A]]) {

	def *(p2: Product[A]): Product[A] = s.append(this, p2)
}
object Product {
	def empty[A: Numeric](implicit s: Monoid[Product[A]]): Product[A] = s.empty
}
