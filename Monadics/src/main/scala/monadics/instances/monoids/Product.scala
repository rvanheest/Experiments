package monadics.instances.monoids

import monadics.structures.Monoid

case class Product[A](product: A)(implicit numeric: Numeric[A], monoid: Monoid[Product[A]]) {

  def *(other: => Product[A]): Product[A] = {
    monoid.combine(this, other)
  }
}

object Product {
  def empty[A](implicit numeric: Numeric[A], monoid: Monoid[Product[A]]): Product[A] = {
    monoid.empty
  }

  implicit def ProductIsMonoid[A](implicit numeric: Numeric[A]) = new Monoid[Product[A]] { self =>
    implicit val selfMonoid: Monoid[Product[A]] = self

    def empty: Product[A] = {
      Product(numeric.one)
    }

    def combine(a1: Product[A], a2: => Product[A]): Product[A] = {
      Product(numeric.times(a1.product, a2.product))
    }
  }
}
