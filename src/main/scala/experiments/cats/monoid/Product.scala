package experiments.cats.monoid

import cats.Monoid

case class Product[A](product: A)

object Product {
  implicit def productIsNumeric[A: Numeric]: Numeric[Product[A]] = new Numeric[Product[A]] {

    private val n = implicitly[Numeric[A]]

    def plus(x: Product[A], y: Product[A]): Product[A] = Product(n.plus(x.product, y.product))

    def minus(x: Product[A], y: Product[A]): Product[A] = Product(n.minus(x.product, y.product))

    def times(x: Product[A], y: Product[A]): Product[A] = Product(n.times(x.product, y.product))

    def negate(x: Product[A]): Product[A] = Product(n.negate(x.product))

    def fromInt(x: Int): Product[A] = Product(n.fromInt(x))

    def toInt(x: Product[A]): Int = n.toInt(x.product)

    def toLong(x: Product[A]): Long = n.toLong(x.product)

    def toFloat(x: Product[A]): Float = n.toFloat(x.product)

    def toDouble(x: Product[A]): Double = n.toDouble(x.product)

    def compare(x: Product[A], y: Product[A]): Int = n.compare(x.product, y.product)
  }

  implicit def productIsMonoid[A: Numeric]: Monoid[Product[A]] = new Monoid[Product[A]] {

    private val n = implicitly[Numeric[A]]

    import n._

    def empty: Product[A] = Product(n.zero)

    def combine(x: Product[A], y: Product[A]): Product[A] = Product(x.product + y.product)
  }
}
