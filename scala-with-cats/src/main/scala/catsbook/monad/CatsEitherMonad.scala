package catsbook.monad

import cats.syntax.either._

import scala.util.Try

object CatsEitherMonad extends App {

  val a = 3.asRight[String]
  val b = 4.asRight[String]

  println {
    for {
      x <- a
      y <- b
    } yield x * x + y * y
  } // Right(25)

  def countPositive(nums: List[Int]) = nums.foldLeft(0.asRight[String]) {
    case (accumulator, num) if num > 0 => accumulator.map(_ + 1)
    case _ => "Negative. Stopping!".asLeft[Int]
  }

  println(countPositive(List(1, 2, 3))) // Right(3)
  println(countPositive(List(1, -2, 3))) // Left("Negative. Stopping!")

  println(Either.catchOnly[NumberFormatException]("foo".toInt)) // Left("java.lang.NumberFormatException: For input string: "foo"")
  println(Either.catchNonFatal(sys.error("Badness"))) // Left("java.lang.RuntimeException: Badness")
  println(Either.fromTry(Try { "foo".toInt })) // Left("java.lang.NumberFormatException: For input string: "foo"")
  println(Either.fromOption(None, "Badness")) // Left("Badness")

  println("Error".asLeft[Int].getOrElse(0)) // 0
  println("Error".asLeft[Int].orElse(2.asRight[String])) // Right(2)
  println((-1).asRight[String].ensure("Must be non-negative!")(_ > 0)) // Left("Must be non-negative!")
  println {
    "error".asLeft[Int].recover {
      case _: String => -1
    }
  } // Right(-1)
  println {
    "error".asLeft[Int].recoverWith {
      case _: String => (-1).asRight[String]
    }
  } // Right(-1)
  println("foo".asLeft[Int].leftMap(_.reverse)) // Left("oof")
  println(6.asRight[String].bimap(_.reverse, _ * 7)) // Right(42)
  println("bar".asLeft[String].bimap(_.reverse, _ * 7)) // Left("rab")
  println(123.asRight.swap) // Left(123)

  sealed trait LoginError extends Product with Serializable
  case class UserNotFound(username: String) extends LoginError
  case class PasswordIncorrect(username: String) extends LoginError
  case object UnexpectedError extends LoginError

  case class User(username: String, password: String)
  type LoginResult = Either[LoginError, User]

  def handleError(error: LoginError): Unit = error match {
    case UserNotFound(username) => println(s"user not found: $username")
    case PasswordIncorrect(username) => println(s"password incorrect: $username")
    case UnexpectedError => println("unexpected error")
  }

  val result1: LoginResult = User("dave", "passw0rd").asRight
  val result2: LoginResult = UserNotFound("dave").asLeft

  println(result1.fold(handleError, println)) // User(dave, passw0rd)
  println(result2.fold(handleError, println)) // User not found: dave
}
