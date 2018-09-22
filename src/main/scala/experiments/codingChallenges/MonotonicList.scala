package experiments.codingChallenges

import scala.annotation.tailrec

object MonotonicList extends App {

  val input1 = List(1, 2, 5, 5, 8) // true
  val input2 = List(9, 4, 4, 2, 2) // true
  val input3 = List(1, 4, 6, 3) // false
  val input4 = List(1, 1, 1, 1) // true

  println(isMonotonic(input1))
  println(isMonotonic(input2))
  println(isMonotonic(input3))
  println(isMonotonic(input4))

  def isMonotonic(input: List[Int]): Boolean = {
    sealed abstract class State
    case object Inc extends State
    case object Dec extends State
    case object Unknown extends State

    @tailrec
    def recursion(input: List[Int], state: State = Unknown): Boolean = {
      input match {
        case Nil | _ :: Nil => true
        case x1 :: x2 :: xs =>
          state match {
            case Unknown =>
              recursion(x2 :: xs, if (x1 < x2) Inc
                                  else if (x1 > x2) Dec
                                  else Unknown)
            case Inc if x1 <= x2 => recursion(x2 :: xs, Inc)
            case Inc => false
            case Dec if x1 >= x2 => recursion(x2 :: xs, Dec)
            case Dec => false
          }
      }
    }

    recursion(input)
  }
}
