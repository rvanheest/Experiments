package catsbook.semigroupal

import cats.Semigroupal
import cats.instances.either._
import cats.syntax.apply._
import cats.syntax.either._

object SemigroupalEither extends App {

  // Note: Semigroupal with Either is failfast as well, it's not accumulating failures.

  case class Cat(name: String, born: Int, color: String)
  type ErrorOr[A] = Either[String, A]

  val catArgs: (ErrorOr[String], ErrorOr[Int], ErrorOr[String]) = ("Tom".asRight[String], 3.asRight[String], "grey".asRight[String])
  println(catArgs.mapN(Cat)) // Right(Cat("Tom", 3, "grey"))

  val catsLeft: (ErrorOr[String], ErrorOr[Int], ErrorOr[String]) = ("Tom".asRight[String], "fail1".asLeft[Int], "fail2".asLeft[String])
  println(catsLeft.mapN(Cat)) // Left("fail1")

  type ErrorsOr[A] = Either[Vector[String], A]

  println {
    Semigroupal[ErrorsOr].product(
      Vector("Error 1").asLeft[Int],
      Vector("Error 2").asLeft[Int],
    )
  } // Left(Vector("Erorr 1"))
}
