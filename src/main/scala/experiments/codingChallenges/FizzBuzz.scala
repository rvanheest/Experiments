package experiments.codingChallenges

/**
 * inspired by https://www.youtube.com/watch?v=QPZ0pIK_wsc
 */
object FizzBuzz extends App {

  val specials = Map(
    3 -> "Fizz",
    5 -> "Buzz"
  ).map((special _).tupled)

  (1 to 100).map(convert).foreach(println)

  def convert(i: Int): String = {
    (specials :\ "")(_(i) + _) match {
      case "" => i.toString
      case s => s
    }
  }

  def special(divider: Int, name: String): Int => String = {
    i => if (i % divider == 0) name else ""
  }
}
