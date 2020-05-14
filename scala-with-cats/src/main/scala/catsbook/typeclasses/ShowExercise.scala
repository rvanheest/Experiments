package catsbook.typeclasses

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._

object ShowExercise extends App {

  case class Cat(name: String, age: Int, color: String)
  object Cat {
    implicit val showCat: Show[Cat] = cat => s"${ cat.name.show } is a ${ cat.age.show } year-old ${ cat.color.show } cat."
  }

  val cat = Cat("Alice", 2, "black")
  println(cat.show)
}
