package catsbook.semigroupal

import cats.Semigroupal
import cats.data.Validated
import cats.data.NonEmptyVector
import cats.instances.list._
import cats.instances.vector._
import cats.instances.string._
import cats.syntax.validated._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.applicativeError._

import scala.util.Try

object CatsValidated extends App {

  type AllErrorsOr[A] = Validated[List[String], A]

  println {
    Semigroupal[AllErrorsOr].product(
      List("Error 1").invalid[Int],
      List("Error 2").invalid[Int],
    )
  } // Invalid(List("Error 1", "Error 2"))

  println(Validated.valid[List[String], Int](123)) // Valid(123)
  println(Validated.invalid[List[String], Int](List("Badness"))) // Invalid(List("Badness"))

  println(123.valid[List[String]]) // Valid(123)
  println(List("Badness").invalid[Int]) // Invalid(List("Badness"))

  println(123.pure[AllErrorsOr]) // Valid(123)
  println(List("Badness").raiseError[AllErrorsOr, Int]) // Invalid(List("Badness"))

  println(Validated.catchOnly[NumberFormatException]("foo".toInt)) // Invalid(java.lang.NumberFormatException: For input string: "foo")

  println(Validated.catchNonFatal(sys.error("Badness"))) // Invalid(java.lang.RuntimeException: "Badness")

  println(Validated.fromTry(Try { "foo".toInt })) // Invalid(java.lang.NumberFormatException: For input string: "foo")

  println(Validated.fromEither(Left("Badness"))) // Invalid("Badness")

  println(Validated.fromOption(None, "Badness")) // Invalid("Badness")

  type StringOr[A] = Validated[String, A]

  println {
    Semigroupal[StringOr].product(
      "Error 1".invalid[Int],
      "Error 2".invalid[Int],
    )
  } // Invalid("Error 1Error 2")

  println {
    (
      "Error 1".invalid[Int]: StringOr[Int],
      "Error 2".invalid[Int]: StringOr[Int],
    ).tupled
  } // Invalid("Error 1Error 2")

  type VectorOr[A] = Validated[Vector[Int], A]

  println {
    (
      Vector(404).invalid[Int]: VectorOr[Int],
      Vector(500).invalid[Int]: VectorOr[Int],
    ).tupled
  } // Invalid(Vector(404, 500))

  type NonEmptyVectorOr[A] = Validated[NonEmptyVector[String], A]

  println {
    (
      NonEmptyVector.of("Error 1").invalid[Int]: NonEmptyVectorOr[Int],
      NonEmptyVector.of("Error 2").invalid[Int]: NonEmptyVectorOr[Int],
    ).tupled
  } // Invalid(NonEmptyVector("Error 1", "Error 2"))

  println(123.valid.map(_ * 100)) // Valid(12300)

  println("?".invalid.leftMap(_ + "??")) // Invalid("???")

  println(123.valid[String].bimap(_ + "!", _ * 100)) // Valid(12300)
  println("?".invalid[Int].bimap(_ + "!", _ * 100)) // Invalid("?!")

  println {
    32.valid.andThen(a => 10.valid.map(b => a + b))
  } // Valid(42)

  // notice that `andThen` is failfast!
  println {
    "fail 1".invalid[Int].andThen(a => "fail2".invalid[Int].map(b => a + b))
  } // Invalid("fail 1")


}
