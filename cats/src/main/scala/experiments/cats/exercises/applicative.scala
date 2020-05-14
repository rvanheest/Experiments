package experiments.cats.exercises

import cats._
import cats.implicits._

object applicative extends App {

  println(Applicative[Option].pure(1))
  println(Applicative[List].pure(1))
  println

  println((Applicative[List] compose Applicative[Option]).pure(1))
  println

  println(Monad[Option].pure(1))
  println(Applicative[Option].pure(1))
}
