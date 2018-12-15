package catsbook.traversable

import cats.Applicative
import cats.instances.option._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.option._

object TraversingOptionExcercise extends App {

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F])((accum, item) => (accum, func(item)).mapN(_ :+ _))
  }

  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] = listTraverse(list)(identity)

  def process(inputs: List[Int]): Option[List[Int]] = {
    listTraverse(inputs) {
      case n if n % 2 == 0 => n.some
      case _ => none
    }
  }

  println(process(List(2, 4, 6))) // Some(List(2, 4, 6))
  println(process(List(1, 2, 3))) // None
}
