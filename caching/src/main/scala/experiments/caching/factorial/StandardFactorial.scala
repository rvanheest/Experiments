package experiments.caching.factorial

object StandardFactorial extends App {

  def factorial(n: Int): Int = {
    def recursive(x: Int): Int = {
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
}
