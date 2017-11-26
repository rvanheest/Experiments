package experiments.codingChallenges

/**
 * inspired by https://www.youtube.com/watch?v=QPZ0pIK_wsc
 */
object FizzBuzz extends App {

  (1 to 100).map(convert).foreach(println)

  def convert(i: Int): String = {
    val fs = List(special(3, "Fizz"), special(5, "Buzz"))
    (fs :\ "")(_(i) + _) match {
      case "" => i.toString
      case s => s
    }
  }

  def special(divider: Int, name: String): Int => String = {
    i => if (i % divider == 0) name else ""
  }
}
