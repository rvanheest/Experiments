package experiments.cats.exercises

import cats.data.{Validated, ValidatedNel}
import cats.implicits._

object traverse extends App {

    def parseIntEither(s: String): Either[NumberFormatException, Int] = Either.catchOnly[NumberFormatException](s.toInt)

  def parseIntValidated(s: String): ValidatedNel[NumberFormatException, Int] = Validated.catchOnly[NumberFormatException](s.toInt).toValidatedNel

  println(List("1", "2", "3").traverse[Either[NumberFormatException, ?], Int](parseIntEither))
  println(List("1", "abc", "3").traverse[Either[NumberFormatException, ?], Int](parseIntEither).isLeft)
  println

  println(List("1", "2", "3").traverse[ValidatedNel[NumberFormatException, ?], Int](parseIntValidated).isValid)
  println

  println(List(Option(1), Option(2), Option(3)).traverse[Option, Int](x => x))
  println(List(Option(1), None, Option(3)).traverse[Option, Int](x => x))
  println

  println(List(Option(1), Option(2), Option(3)).sequence)
  println(List(Option(1), None, Option(3)).sequence)
  println

  println(List(Option(1), Option(2), Option(3)).sequence_)
  println(List(Option(1), None, Option(3)).sequence_)
}
