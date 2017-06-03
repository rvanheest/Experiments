package experiments.cats.monoid

import cats.Monoid

case class Count(count: Int)

object Count {
  implicit def countIsNumeric: Numeric[Count] = new Numeric[Count] {
    def plus(x: Count, y: Count): Count = Count(x.count + y.count)

    def minus(x: Count, y: Count): Count = Count(x.count - y.count)

    def times(x: Count, y: Count): Count = Count(x.count * y.count)

    def negate(x: Count): Count = Count(-x.count)

    def fromInt(x: Int): Count = Count(x)

    def toInt(x: Count): Int = x.count

    def toLong(x: Count): Long = x.count.toLong

    def toFloat(x: Count): Float = x.count.toFloat

    def toDouble(x: Count): Double = x.count.toDouble

    def compare(x: Count, y: Count): Int = if (x.count < y.count) -1 else if (x.count == y.count) 0 else 1
  }

  implicit def countIsMonoid: Monoid[Count] = new Monoid[Count] {
    def empty: Count = Count(0)

    def combine(x: Count, y: Count): Count = Count(x.count + y.count)
  }
}
