package catsbook.traversable

import cats.data.ValidatedNec
import cats.instances.list._
import cats.instances.try_._
import cats.syntax.traverse._
import cats.syntax.validated._

import scala.util.Try

object SequenceOfTry2 extends App {

  type ErrorsOr[A] = ValidatedNec[String, A]

  def tryToValidated[T](t: Try[T]): ErrorsOr[T] = t.fold(_.getMessage.invalidNec, _.validNec)

  def parseInt(s: String): Try[Int] = Try { s.toInt }

  def validateParse(s: String): ErrorsOr[Int] = tryToValidated(parseInt(s))

  println(List("123", "456", "789").traverse(parseInt)) // Success(List(123, 456, 789))
  println(List("123", "a456", "b789").traverse(parseInt)) // Failure(java.lang.NumberFormatException: For input string: "a456"

  println(List("123", "456", "789").traverse[ErrorsOr, Int](validateParse)) // Valid(List(123, 456, 789))
  println(List("123", "a456", "b789").traverse[ErrorsOr, Int](validateParse)) // Invalid(Chain(For input string: "a456", For input string: "b789"))

  println(List("123", "456", "789").map(parseInt).traverse[ErrorsOr, Int](tryToValidated)) // Valid(List(123, 456, 789))
  println(List("123", "a456", "b789").map(parseInt).traverse[ErrorsOr, Int](tryToValidated)) // Invalid(Chain(For input string: "a456", For input string: "b789"))
}
