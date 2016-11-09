package experiments.view

sealed abstract class Peano
object Peano extends View[Int, Peano] {

  case object Zero extends Peano
  case class Succ(n: Int) extends Peano

  implicit def in(n: Int): Peano =
    if (n == 0) Zero
    else Succ(n - 1)

  implicit def out(view: Peano): Int = view match {
    case Zero => 0
    case Succ(n) => n + 1
  }
}

object Factorial extends App {

  import Peano._

  def fib(x: Int): Int = {
    implicitly[Peano](x) match {
      case Zero => Zero
      case s@Succ(n) => implicitly[Peano](n) match {
        case Zero => Succ(Zero)
        case Succ(m) => fib(m) + fib(Succ(m))
      }
    }
  }

  (0 to 10).map(fib).foreach(println)
}
