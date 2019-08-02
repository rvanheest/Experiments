package experiments.cats.monoid

import cats.Monoid
import cats.implicits._

case class Average[A](sum: Sum[A], count: Count) {
  def this(a: A, n: Int) = this(Sum(a), Count(n))

  def this(tuple: (Sum[A], Count)) = this(tuple._1, tuple._2)

  def tupled: (Sum[A], Count) = (sum, count)
}

object Average {
  def create[A](x: A): Average[A] = new Average(x, 1)

  implicit def averageIsMonoid[A: Numeric]: Monoid[Average[A]] = new Monoid[Average[A]] {
    def empty: Average[A] = {
      new Average(Monoid[(Sum[A], Count)].empty)
    }

    def combine(x: Average[A], y: Average[A]): Average[A] = {
      new Average(x.tupled |+| y.tupled)
    }
  }

  def average[A: Fractional](average: Average[A]): A = {
    val f = implicitly[Fractional[A]]
    import f._
    average.sum.sum / f.fromInt(average.count.count)
  }

  def calculateAverage[A: Fractional](as: List[A]): A = average(as.foldMap(create))
}
