package experiments.cats.monoid

import cats.Monoid

case class Sum[A](sum: A)

object Sum {
  implicit def sumIsNumeric[A: Numeric]: Numeric[Sum[A]] = new Numeric[Sum[A]] {

    private val n = implicitly[Numeric[A]]

    def plus(x: Sum[A], y: Sum[A]): Sum[A] = Sum(n.plus(x.sum, y.sum))

    def minus(x: Sum[A], y: Sum[A]): Sum[A] = Sum(n.minus(x.sum, y.sum))

    def times(x: Sum[A], y: Sum[A]): Sum[A] = Sum(n.times(x.sum, y.sum))

    def negate(x: Sum[A]): Sum[A] = Sum(n.negate(x.sum))

    def fromInt(x: Int): Sum[A] = Sum(n.fromInt(x))

    def toInt(x: Sum[A]): Int = n.toInt(x.sum)

    def toLong(x: Sum[A]): Long = n.toLong(x.sum)

    def toFloat(x: Sum[A]): Float = n.toFloat(x.sum)

    def toDouble(x: Sum[A]): Double = n.toDouble(x.sum)

    def compare(x: Sum[A], y: Sum[A]): Int = n.compare(x.sum, y.sum)
  }

  implicit def sumIsMonoid[A: Numeric]: Monoid[Sum[A]] = new Monoid[Sum[A]] {

    private val n = implicitly[Numeric[A]]

    import n._

    def empty: Sum[A] = Sum(n.zero)

    def combine(x: Sum[A], y: Sum[A]): Sum[A] = Sum(x.sum + y.sum)
  }
}
