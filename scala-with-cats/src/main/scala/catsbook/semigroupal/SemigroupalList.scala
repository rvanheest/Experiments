package catsbook.semigroupal

import cats.Semigroupal
import cats.instances.list._

object SemigroupalList extends App {

  /*
   * Note that Semigroupal[List].product gives the same result as flatMapping!
   */

  println(Semigroupal[List].product(List(1, 2), List(3, 4))) // List((1, 3), (1, 4), (2, 3), (2, 4))

  println {
    for {
      x <- List(1, 2)
      y <- List(3, 4)
    } yield (x, y)
  } // List((1, 3), (1, 4), (2, 3), (2, 4))
}
