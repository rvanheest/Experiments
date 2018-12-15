package catsbook.traversable

import cats.data.Validated
import cats.instances.list._
import cats.instances.try_._
import cats.syntax.applicative._
import cats.syntax.traverse._
import cats.syntax.validated._

import scala.util.Try

object SequenceOfTry extends App {

  type ErrorsOr[A] = Validated[List[String], A]

  def tryToValidated[T](t: Try[T]): ErrorsOr[T] = t.fold(_.getMessage.pure[List].invalid, _.valid)

  def parseInt(s: String): Try[Int] = Try { s.toInt }

  def validateParse(s: String): ErrorsOr[Int] = tryToValidated(parseInt(s))

  println(List("123", "456", "789").traverse(parseInt)) // Success(List(123, 456, 789))
  println(List("123", "a456", "b789").traverse(parseInt)) // Failure(java.lang.NumberFormatException: For input string: "a456"

  println(List("123", "456", "789").traverse[ErrorsOr, Int](validateParse)) // Valid(List(123, 456, 789))
  println(List("123", "a456", "b789").traverse[ErrorsOr, Int](validateParse)) // Invalid(List(For input string: "a456", For input string: "b789"))

  println(List("123", "456", "789").map(parseInt).traverse[ErrorsOr, Int](tryToValidated)) // Valid(List(123, 456, 789))
  println(List("123", "a456", "b789").map(parseInt).traverse[ErrorsOr, Int](tryToValidated)) // Invalid(List(For input string: "a456", For input string: "b789"))
}
