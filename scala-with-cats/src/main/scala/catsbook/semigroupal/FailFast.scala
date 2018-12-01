package catsbook.semigroupal

import cats.syntax.either._

object FailFast extends App {

  def parseInt(str: String): Either[String, Int] = {
    Either.catchOnly[NumberFormatException](str.toInt)
      .leftMap(_ => s"Couldn't read $str")
  }

  println {
    for {
      a <- parseInt("a")
      b <- parseInt("b")
      c <- parseInt("c")
    } yield a + b + c
  } // Left("Couldn't read a")
}
