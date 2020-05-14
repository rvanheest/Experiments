package catsbook.monad

import cats.{ Id, Monad }
import cats.syntax.flatMap._
import cats.syntax.functor._

object CatsIdentityMonad extends App {

  def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = {
    for {
      x <- a
      y <- b
    } yield x * x + y * y
  }

  //  this doesn't compile! Int is not a Monad
  //  sumSquare(3, 4)

  println(sumSquare(3: Id[Int], 4: Id[Int])) // 25

  val a = Monad[Id].pure(3)
  val b = Monad[Id].flatMap(a)(_ + 1)

  println {
    for {
      x <- a
      y <- b
    } yield x + y
  } // 7
}
