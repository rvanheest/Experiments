package catsbook.functor

import cats.Monoid
import cats.instances.string._
import cats.syntax.semigroup._
import cats.syntax.invariant._

object CatsInvariantFunctor extends App {

  implicit val symbolMonoid: Monoid[Symbol] = Monoid[String].imap(Symbol.apply)(_.name)

  println(Monoid[Symbol].empty) // '
  println('a |+| 'few |+| 'words) // 'afewwords
}
