package catsbook.monoid

import cats.{ Monoid, Semigroup }
import cats.instances.int._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.semigroup._

object CatsMonoids extends App {

  println(s"'${ Monoid[String].combine("Hi ", "there") }'")
  println(s"'${ Monoid[String].empty }'")

  println(s"'${ Semigroup[String].combine("Hi ", "there") }'")

  println(s"'${ Monoid[Int].combine(32, 10) }'")

  println(s"'${ Monoid[Option[Int]].combine(Option(22), Option(20)) }'")

  // using syntax
  println(s"'${ "Hi " |+| "there" |+| Monoid[String].empty }'")

  println(s"'${ 1 |+| 2 |+| Monoid[Int].empty }'")
}
