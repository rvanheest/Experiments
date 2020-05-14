package catsbook.semigroupal

import cats.data.Validated
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.apply._

object FormValidationFailFastExercise extends App {

  case class User(name: String, age: Int)

  type InputData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def getValue(name: String)(data: InputData): FailFast[String] = {
    data.get(name)
      .toRight(List(s"$name field not specified"))
  }

  def parseInt(name: String)(data: String): FailFast[Int] = {
    Either.catchOnly[NumberFormatException] { data.toInt }
      .leftMap(_ => List(s"$name must be an integer"))
  }

  def nonBlank(name: String)(data: String): FailFast[String] = {
    data.asRight[List[String]]
      .ensure(List(s"$name cannot be blank"))(_.trim.nonEmpty)
  }

  def nonNegative(name: String)(data: Int): FailFast[Int] = {
    data.asRight[List[String]]
      .ensure(List(s"$name cannot be negative"))(_ >= 0)
  }

  def readName(data: InputData): FailSlow[String] = {
    getValue("name")(data)
      .flatMap(nonBlank("name"))
      .toValidated
  }

  def readAge(data: InputData): FailSlow[Int] = {
    getValue("age")(data)
      .flatMap(nonBlank("age"))
      .flatMap(parseInt("age"))
      .flatMap(nonNegative("age"))
      .toValidated
  }

  def readUser(data: InputData): FailSlow[User] = {
    (
      readName(data),
      readAge(data),
    ).mapN(User)
  }

  println {
    readUser(Map(
      "name" -> "Dave",
      "age" -> "37",
    ))
  } // Valid(User("Dave", 37))

  println {
    readUser(Map(
      "age" -> "-1",
    ))
  } // Invalid(List("name field not specified", "age cannot be negative"))
}
