package experiments.view

import scala.language.implicitConversions

sealed abstract class Peano
object Peano extends View[Int, Peano] {

  case object Zero extends Peano
  case class Succ(n: Int) extends Peano

  /**
   * {{{
   * in n = Zero,         if n = 0
   *      = Suc (n - 1),  if n > 0
   * }}}
   */
  implicit def in(n: Int): Peano =
    if (n == 0) Zero
    else Succ(n - 1)

  /**
   * {{{
   * out Zero     = 0
   * out (Succ n) = n + 1
   * }}}
   */
  implicit def out(view: Peano): Int = view match {
    case Zero => 0
    case Succ(n) => n + 1
  }
}

object FactorialDemo extends App {

  import Peano._

  /**
   * {{{
   * fib Zero            = Zero
   * fib (Succ Zero)     = Succ Zero
   * fib (Succ (Succ n)) = (fib n) + (fib (Succ n))
   * }}}
   */
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
