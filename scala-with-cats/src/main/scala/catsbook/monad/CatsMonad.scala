package catsbook.monad

import cats.Monad
import cats.instances.list._
import cats.instances.option._
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._

object CatsMonad extends App {

  val opt1 = Monad[Option].pure(3)
  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
  val opt3 = Monad[Option].map(opt2)(a => 100 * a)
  println(opt3) // Some(500)

  val list1 = Monad[List].pure(3)
  val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
  val list3 = Monad[List].map(list2)(a => a + 123)
  println(list3) // List(124, 133, 125, 143, 126, 153)

  val vector = Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a * 10))
  println(vector) // Vector(1, 10, 2, 20, 3, 30)

  println(1.pure[Option]) // Some(1)
  println(1.pure[List]) // List(1)

  def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = {
    for {
      x <- a
      y <- b
    } yield x * x + y * y
  }

  println(sumSquare(Option(3), Option(4))) // Some(25)
  println(sumSquare(List(1, 2, 3), List(4, 5))) // List(17, 26, 20, 29, 25, 34)
}
