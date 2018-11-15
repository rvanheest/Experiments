package catsbook.typeclasses

import java.util.Date

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._

object ShowTypeClass extends App {

  val showInt: Show[Int] = Show[Int]
  val showString: Show[String] = Show[String]

  println(showInt.show(123))
  println(showString.show("abc"))

  println(123.show)
  println("abc".show)

  implicit val dateShow: Show[Date] = date => s"${ date.getTime } ms since the epoch."
}
