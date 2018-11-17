package catsbook.functor

import cats.Contravariant
import cats.Show
import cats.instances.string._
import cats.syntax.contravariant._

object CatsCovariantFunctor extends App {

  val showString = Show[String]

  val showSymbol = Contravariant[Show]
    .contramap[String, Symbol](showString)(symbol => s"'${symbol.name}")

  println(showSymbol.show('dave)) // 'dave

  println(showString.contramap[Symbol](symbol => s"'${symbol.name}").show('dave)) // 'dave
}
