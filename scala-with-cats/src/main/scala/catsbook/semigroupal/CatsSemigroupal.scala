package catsbook.semigroupal

import cats.Semigroupal
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.option._

object CatsSemigroupal extends App {

  println(Semigroupal[Option].product(123.some, "abc".some)) // Some((123, "abc"))
  println(Semigroupal[Option].product(123.some, none)) // None
  println(Semigroupal[Option].product(none, "abc".some)) // None

  println(Semigroupal.tuple3(1.some, 2.some, 3.some)) // Some((1, 2, 3))
  println(Semigroupal.tuple3(1.some, 2.some, none)) // None

  println(Semigroupal.map3(1.some, 2.some, 3.some)(_ + _ + _)) // Some(6)
  println(Semigroupal.map3(1.some, 2.some, none[Int])(_ + _ + _)) // None

  println((123.some, "abc".some, true.some).tupled) // Some((123, "abc", true))
  println((1.some, 2.some, 3.some).mapN(_ + _ + _)) // Some(6)
}
