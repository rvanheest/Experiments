package catsbook.monad

import cats.MonadError
import cats.instances.either._
import cats.instances.try_._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.monadError._

import scala.util.Try

object CatsMonadError extends App {

  type ErrorOr[A] = Either[String, A]

  val monadError = MonadError[ErrorOr, String]

  val success = monadError.pure(42)
  val failure = monadError.raiseError("Badness")

  println {
    monadError.handleErrorWith(failure) {
      case "Badness" => monadError.pure("It's ok")
      case _ => monadError.raiseError("It's not ok")
    }
  } // Right("It's ok")

  println(monadError.ensure(success)("Number too low!")(_ > 1000)) // Left("Number is too low!")

  val success2 = 42.pure[ErrorOr]
  val failure2 = "Badness".raiseError[ErrorOr, String]

  println {
    failure2.handleErrorWith {
      case "Badness" => "It's ok".pure[ErrorOr]
      case _ => "It's not ok".raiseError[ErrorOr, String]
    }
  } // Right("It's ok")

  println(success2.ensure("Number too low!")(_ > 1000)) // Left("Number too low!")

  val exception: Throwable = new RuntimeException("It's all gone wrong")
  println(exception.raiseError[Try, Int]) // Failure(java.lang.RuntimeException: It's all gone wrong)
}
