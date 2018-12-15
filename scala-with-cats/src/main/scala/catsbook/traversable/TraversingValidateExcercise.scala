package catsbook.traversable

import cats.Applicative
import cats.data.Validated
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.validated._

object TraversingValidateExcercise extends App {

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F])((accum, item) => (accum, func(item)).mapN(_ :+ _))
  }

  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] = listTraverse(list)(identity)

  type ErrorsOr[A] = Validated[List[String], A]

  def process(inputs: List[Int]): ErrorsOr[List[Int]] = {
    listTraverse[ErrorsOr, Int, Int](inputs) {
      case n if n % 2 == 0 => n.valid[List[String]]
      case n => List(s"$n is not even").invalid[Int]
    }
  }

  println(process(List(2, 4, 6))) // Valid(List(2, 4, 6))
  println(process(List(1, 2, 3))) // Invalid(List("1 is not even", "3 is not even"))
}
