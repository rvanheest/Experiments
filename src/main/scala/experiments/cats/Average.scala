package experiments.cats

import cats._
import cats.implicits._
import experiments.cats.Average._
import experiments.cats.Count._
import experiments.cats.Sum._

object Sum {

  case class Sum[A](sum: A)
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

object Product {

  case class Product[A](product: A)
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

object Count {

  case class Count(count: Int)
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

object Average {

  case class Average[A](sum: Sum[A], count: Count) {
    def this(a: A, n: Int) = this(Sum.Sum(a), Count.Count(n))
  }

  def create[A](x: A): Average[A] = new Average(x, 1)

  implicit def averageIsMonoid[A: Numeric]: Monoid[Average[A]] = new Monoid[Average[A]] {
    def empty: Average[A] = {
      Average(Monoid[Sum[A]].empty, Monoid[Count].empty)
    }

    def combine(x: Average[A], y: Average[A]): Average[A] = {
      Average(x.sum |+| y.sum, x.count |+| y.count)
    }
  }

  def average[A: Fractional](average: Average[A]): A = {
    val f = implicitly[Fractional[A]]
    import f._
    average.sum.sum / f.fromInt(average.count.count)
  }

  def calculateAverage[A: Fractional](as: List[A]): A = average(as.foldMap(create))
}

object Variance {
  case class Variance[A](sumOfSquares: Sum[A], sum: Sum[A], count: Count)

  def create[A: Numeric](x: A): Variance[A] = {
    Variance(Sum.Sum(implicitly[Numeric[A]].times(x, x)), Sum.Sum(x), Count.Count(1))
  }

  implicit def varianceIsMonoid[A: Numeric]: Monoid[Variance[A]] = new Monoid[Variance[A]] {
    def empty: Variance[A] = {
      Variance(Monoid[Sum[A]].empty, Monoid[Sum[A]].empty, Monoid[Count].empty)
    }

    def combine(x: Variance[A], y: Variance[A]): Variance[A] = {
      val sos = x.sumOfSquares |+| y.sumOfSquares
      val sum = x.sum |+| y.sum
      val count = x.count |+| y.count

      Variance(sos, sum, count)
    }
  }

  def variance[A: Fractional](variance: Variance[A]): A = {
    val f = implicitly[Numeric[A]]
    import f._
    import Average.average

    val pt1 = average(Average.Average(variance.sumOfSquares, variance.count))
    val pt2 = average(Average.Average(variance.sum, variance.count))

    pt1 - (pt2 * pt2)
  }

  def calculateVariance[A: Fractional](as: List[A]): A = variance(as.foldMap(x => create(x)))
}

object Main {
  def main(args: Array[String]): Unit = {
    val xs = (1 to 9).map(_.toDouble).toList
    println(Average.calculateAverage(xs))
    println(Variance.calculateVariance(xs))
  }
}
