package experiments.codingChallenges

// find the least disruptive replacement for a list of numbers
object LeastDisruptiveSubrange extends App {

  val original = List(1, 2, 3, 4, 5)
  val replacement = List(3, 5, 3)

  println(findLeastDisruptiveSubrangeIndex(original, replacement))

  def findLeastDisruptiveSubrangeIndex(original: List[Int], replacement: List[Int]): Int = {
    original.zipWithIndex
      .tails
      .collect {
        case list @ (_, index) :: _ if list.size >= replacement.size =>
          val distanceSum = list.take(replacement.size)
            .zip(replacement)
            .map { case ((num, _), repl) => (num - repl).abs }
            .sum

          (distanceSum, index)
      }
      .minBy { case (sum, _) => sum }
      ._2
  }
}
