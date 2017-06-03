package experiments.cats.monoid

import cats.Monoid
import cats.implicits._

case class Variance[A](sumOfSquares: Sum[A], sum: Sum[A], count: Count) {
  def this(tuple: (Sum[A], Sum[A], Count)) = this(tuple._1, tuple._2, tuple._3)
  def tupled: (Sum[A], Sum[A], Count) = (sumOfSquares, sum, count)
}

object Variance {
  def create[A: Numeric](x: A): Variance[A] = {
    Variance(Sum(implicitly[Numeric[A]].times(x, x)), Sum(x), Count(1))
  }

  implicit def varianceIsMonoid[A: Numeric]: Monoid[Variance[A]] = new Monoid[Variance[A]] {
    def empty: Variance[A] = {
      new Variance(Monoid[(Sum[A], Sum[A], Count)].empty)
    }

    def combine(x: Variance[A], y: Variance[A]): Variance[A] = {
      new Variance(x.tupled |+| y.tupled)
    }
  }

  def variance[A: Fractional](variance: Variance[A]): A = {
    val f = implicitly[Numeric[A]]
    import Average.average
    import f._

    val pt1 = average(Average(variance.sumOfSquares, variance.count))
    val pt2 = average(Average(variance.sum, variance.count))

    pt1 - (pt2 * pt2)
  }

  def calculateVariance[A: Fractional](as: List[A]): A = variance(as.foldMap(x => create(x)))
}
