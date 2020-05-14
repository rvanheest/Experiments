package catsbook

import cats.data.{ Ior, NonEmptyChain }
import cats.instances.list._
import cats.syntax.apply._
import cats.syntax.ior._
import cats.syntax.traverse._

object CatsIor extends App {

  val rightIorInt = 3.rightIor[String]
  val leftIorString = "err".leftIor[Int]
  val bothIor = Ior.both("err", 3)

  println(rightIorInt)
  println(leftIorString)
  println(bothIor)

  type Failures = NonEmptyChain[String]
  val Failures = NonEmptyChain

  case class Username(value: String) extends AnyVal
  case class Password(value: String) extends AnyVal

  case class User(name: Username, pw: Password)

  def validateUsername(u: String): Failures Ior Username = {
    if (u.isEmpty)
      Failures.one("Can't be empty").leftIor
    else if (u contains ".")
           Ior.both(Failures.one("Dot in name is deprecated"), Username(u))
    else
      Username(u).rightIor
  }

  def validatePassword(p: String): Failures Ior Password = {
    if (p.length < 8)
      Failures.one("Password too short").leftIor
    else if (p.length < 10)
           Ior.both(Failures.one("Password should be longer"), Password(p))
    else
      Password(p).rightIor
  }

  def validateUser(name: String, password: String): Failures Ior User = {
    (validateUsername(name), validatePassword(password)).mapN(User)
  }

  println(validateUser("John", "password12")) // right
  println(validateUser("john.doe", "password")) // both
  println(validateUser("jane", "short")) // left

  // traverse on an Ior
  val right = ("John", "password12")
  val both = ("john.doe", "password")
  val left = ("jane", "short")

  println(List(right, both).traverse { case (name, pw) => validateUser(name, pw) }) // both
  println(List(right, left).traverse { case (name, pw) => validateUser(name, pw) }) // left
  println(List(both, left).traverse { case (name, pw) => validateUser(name, pw) }) // left
}
