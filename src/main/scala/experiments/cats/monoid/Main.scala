package experiments.cats.monoid

object Main {
  def main(args: Array[String]): Unit = {
    val xs = (1 to 9).map(_.toDouble).toList
    println(Average.calculateAverage(xs))
    println(Variance.calculateVariance(xs))
  }
}
