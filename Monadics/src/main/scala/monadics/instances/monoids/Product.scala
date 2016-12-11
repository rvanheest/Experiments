package monadics.instances.monoids

import monadics.structures.{Equals, Monoid}

case class Product[A](product: A)(implicit monoid: Monoid[Product[A]]) {

  def *(other: => Product[A]): Product[A] = monoid.combine(this, other)
}

object Product {
  def empty[A](implicit monoid: Monoid[Product[A]]): Product[A] = {
    monoid.empty
  }

  implicit def productIsEquals[A](implicit aEquals: Equals[A]): Equals[Product[A]] = {
    Equals.create { case (Product(a1), Product(a2)) => aEquals.equals(a1, a2) }
  }

  implicit def productIsMonoid[A](implicit numeric: Numeric[A]): Monoid[Product[A]] = {
    Monoid.create(Product(numeric.one)) {
      case (Product(a1), Product(a2)) => Product(numeric.times(a1, a2))
    }
  }
}
