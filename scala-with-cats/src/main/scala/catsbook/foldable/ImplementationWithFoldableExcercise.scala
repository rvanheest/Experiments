package catsbook.foldable

import scala.math.Numeric._
import scala.Numeric.Implicits._

object ImplementationWithFoldableExcercise extends App {

  def map[A, B](list: List[A])(f: A => B): List[B] = {
    list.foldRight(List.empty[B])(f(_) :: _)
  }

  println(map(List.empty[Int])(_ + 1)) // List()
  println(map(List(1, 2, 3))(_ + 1)) // List(2, 3, 4)

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {
    list.foldRight(List.empty[B])(f(_) ::: _)
  }

  println(flatMap(List.empty[Int])(x => List(x + 1, x + 2))) // List()
  println(flatMap(List(1, 2, 3))(x => List(x + 1, x + 2))) // List(2, 3, 3, 4, 4, 5)

  def filter[A](list: List[A])(predicate: A => Boolean): List[A] = {
    list.foldRight(List.empty[A]) {
      case (x, xs) if predicate(x) => x :: xs
      case (_, xs) => xs
    }
  }

  println(filter(List.empty[Int])(_ % 2 == 1)) // List()
  println(filter(List(1, 2, 3))(_ % 2 == 1)) // List(1, 3)

  def sum[A: Numeric](list: List[A]): A = {
    list.foldRight(implicitly[Numeric[A]].zero)(_ + _)
  }

  println(sum(List.empty[Int])) // 0
  println(sum(List(1, 2, 3))) // 6
}
