package catsbook.foldable

import cats.{ Eval, Foldable }
import cats.instances.list._
import cats.instances.stream._
import cats.instances.option._
import cats.instances.int._
import cats.instances.string._
import cats.instances.vector._
import cats.syntax.foldable._

object CatsFoldable extends App {

  println(Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _)) // 6

  println(Foldable[Option].foldLeft(Option(123), 10)(_ * _)) // 1230

  /*
   * foldLeft on big data works, but foldRight doesn't
   * However, when using Eval in the process, foldRight works as well
   */
  val bigData: Stream[Long] = (1L to 100000L).toStream
  println(bigData.foldLeft(0L)(_ + _)) // 5000050000
//  println(bigData.foldRight(0L)(_ + _)) // StackOverflow
  println(Foldable[Stream].foldRight(bigData, Eval.now(0L))((num, eval) => eval.map(_ + num)).value) // 5000050000

  println(Foldable[Option].nonEmpty(Option(42))) // true

  println(Foldable[List].find(List(1, 2, 3))(_ % 2 == 0)) // Some(2)

  println(Foldable[List].combineAll(List(1, 2, 3))) // 6

  println(Foldable[List].foldMap(List(1, 2, 3))(_.toString)) // "123"

  val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
  println((Foldable[List] compose Foldable[Vector]).combineAll(ints)) // 21

  // using syntax
  println(List(1, 2, 3).combineAll) // 6
  println(List(1, 2, 3).foldMap(_.toString)) // "123"
}
