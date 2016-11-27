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

  implicit def productIsMonoid[A](implicit numeric: Numeric[A]): Monoid[Product[A]] = {
    Monoid.create(Product(numeric.one)) {
      case (Product(a1), Product(a2)) => Product(numeric.times(a1, a2))
    }
  }
}
