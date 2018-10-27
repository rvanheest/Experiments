package experiments.codingChallenges

object BridgeAndTorch extends App {

  def solution(input: List[Int]): Int = {
    println(s"input: $input")

    def recursion(sorted: List[Int]): Int = {
      val size = sorted.size

      sorted match {
        case Nil =>
          // no one wants to cross the bridge
          0
        case single :: Nil =>
          // single crossed the bridge
          single
        case first :: second :: Nil =>
          // first and second crosssed the bridge
          second
        case first :: second :: third :: Nil =>
          // first and second crossed the bridge
          // first came back
          // first and third crossed the bridge
          first + second + third
        case first :: second :: rest =>
          val slowest = rest.last
          val secondSlowest = rest.init.last

          val opt1 = first + 2 * second + slowest
          val opt2 = 2 * first + secondSlowest + slowest

          if (opt1 < opt2) {
            opt1 + recursion(sorted.take(size - 2))
          }
          else {
            opt2 + recursion(sorted.take(size - 2))
          }
      }
    }

    val result = recursion(input.sorted)
    println(s"time spend: $result")
    result
  }

  val tests = Seq(
    List(9) -> 9,
    List(1, 2, 5, 8) -> 15,
    List(1, 3, 4, 5) -> 14,
    List(3, 1, 6, 8, 12) -> 29,
    List(1, 5, 1, 1) -> 9,
    List(1, 2, 3, 4, 5, 6) -> 22,
    List(1, 2, 3) -> 6,
    List(1, 2, 3, 4, 5, 6, 7) -> 28
  )

  val failures = tests.collect {
    case (input, expected) if solution(input) != expected =>
      s"$input returns ${ solution(input) }, not $expected"
  }.mkString(", ")

  if (failures.isEmpty)
    println("all succeeded")
  else
    println(s"Failures: $failures")
}
