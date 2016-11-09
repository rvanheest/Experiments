package experiments.view

sealed abstract class OddEven
object OddEven extends View[Int, OddEven] {

  case object Zero extends OddEven
  case class Even(n: Int) extends OddEven
  case class Odd(n: Int) extends OddEven

  implicit def in(n: Int): OddEven =
    if (n == 0) Zero
    else if (n > 0 && n % 2 == 0) Even(n / 2)
    else if (n > 0 && n % 2 == 1) Odd((n - 1) / 2)
    else throw new IllegalArgumentException(s"unsupported number: $n")

  implicit def out(v: OddEven): Int = v match {
    case Zero => 0
    case Even(n) if 2 * n > 0 => 2 * n
    case Odd(n) if 2 * n + 1 > 0 => 2 * n + 1
    case other => throw new IllegalArgumentException(s"unsupported view: $other")
  }
}

object Power extends App {

  import OddEven._

  def power(x: Int, y: Int): Int = {
    implicitly[OddEven](y) match {
      case Zero => 1
      case Even(n) => power(x * x, n)
      case Odd(n) => x * power(x * x, n)
    }
  }

  println(power(2, 0))
  println(power(2, 1))
  println(power(2, 2))
  println(power(2, 3))

  println(power(0, 10))
  println(power(10, 0))
}
