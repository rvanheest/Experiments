package catsbook.traversable

import cats.Applicative
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.apply._

object TraversingVectorsExercise extends App {

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F])((accum, item) => (accum, func(item)).mapN(_ :+ _))
  }

  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] = listTraverse(list)(identity)

  // cross product of vector elements
  println(listSequence(List(Vector(1, 2), Vector(3, 4)))) // Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
  println(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))) // Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6))
}
