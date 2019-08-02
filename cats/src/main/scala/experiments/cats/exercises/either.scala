package experiments.cats.exercises

import cats.implicits._

object either extends App {

  println(Either.right[String, Int](5).map(_ + 1))

  println(Either.left[String, Int]("Something went wrong").map(_ + 1))

  println(Either.right[String, Int](5).flatMap(x => Either.right(x + 1)))

  println(Either.left[String, Int]("Something went wrong").flatMap(x => Either.right(x + 1)))

  object EitherStyle {
    def parse(s: String): Either[NumberFormatException, Int] =
      if (s.matches("-?[0-9]+")) Either.right(s.toInt)
      else Either.left(new NumberFormatException(s"$s is not a valid integer."))

    def reciprocal(i: Int): Either[IllegalArgumentException, Double] =
      if (i == 0) Either.left(new IllegalArgumentException("Cannot take reciprocal of 0."))
      else Either.right(1.0 / i)

    def stringify(d: Double): String = d.toString

    def magic(s: String): Either[Exception, String] =
      parse(s).flatMap(reciprocal).map(stringify)
  }

  import EitherStyle._

  println(parse("Not a number").isRight)
  println(parse("2").isRight)

  println(magic("0").isRight)
  println(magic("1").isRight)
  println(magic("Not a number").isRight)

  println(magic("2") match {
    case Left(_: NumberFormatException) => "Not a number!"
    case Left(_: IllegalArgumentException) => "Can't take reciprocal of 0!"
    case Left(_) => "Unknown error"
    case Right(result) => s"Got reciprocal: $result"
  })

  object EitherStyleWithAdts {
    sealed abstract class Error
    final case class NotANumber(string: String) extends Error
    final case object NoZeroReciprocal extends Error

    def parse(s: String): Either[Error, Int] =
      if (s.matches("-?[0-9]+")) Either.right(s.toInt)
      else Either.left(NotANumber(s))

    def reciprocal(i: Int): Either[Error, Double] =
      if (i == 0) Either.left(NoZeroReciprocal)
      else Either.right(1.0 / i)

    def stringify(d: Double): String = d.toString

    def magic(s: String): Either[Error, String] =
      parse(s).flatMap(reciprocal).map(stringify)
  }

  import EitherStyleWithAdts.{NoZeroReciprocal, NotANumber}

  println(EitherStyleWithAdts.magic("2") match {
    case Left(NotANumber(_)) ⇒ "Not a number!"
    case Left(NoZeroReciprocal) ⇒ "Can't take reciprocal of 0!"
    case Right(result) ⇒ s"Got reciprocal: $result"
  })

  println(Either.right(41).map(_ + 1))
  println(Either.left[String, Int]("Hello").map(_ + 1))
  println(Either.left[String, Int]("Hello").leftMap(_.reverse))

  println(Either.catchOnly[NumberFormatException]("abc".toInt).isRight)
  println(Either.catchNonFatal(1 / 0).isLeft)

  println(42.asRight[String])
}
