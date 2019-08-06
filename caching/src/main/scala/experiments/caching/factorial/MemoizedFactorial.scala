package experiments.caching.factorial

import scalacache._
import scalacache.guava._
import scalacache.memoization._
import scalacache.modes.sync._

object MemoizedFactorial extends App {

  implicit val guavaCache: Cache[Int] = GuavaCache[Int]

  def factorial(n: Int): Int = {
    println(s"factorial($n)")
    
    def recursive(x: Int): Int = memoizeSync(None) {
      println(s"recursive($x)")
      x match {
        case 0 => 0
        case 1 => 1
        case _ => recursive(x - 1) + recursive(x - 2)
      }
    }

    recursive(n)
  }

  println(factorial(5))
  println(factorial(4))
  println(factorial(3))
  println(factorial(2))
  println(factorial(1))
  println(factorial(0))
  println(factorial(7))
}
