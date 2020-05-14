package catsbook.semigroupal

import cats.Monoid
import cats.instances.int._
import cats.instances.invariant._
import cats.instances.list._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.apply._
import cats.syntax.option._
import cats.syntax.semigroup._

object ApplySyntax extends App {

  case class Cat(name: String, born: Int, color: String)
  println(("Tom".some, 3.some, "grey".some).mapN(Cat)) // Some(Cat("Tom", 3, "grey"))

  val add: (Int, Int, Int) => Int = _ + _ + _
  println((1.some, 2.some, 3.some).mapN(add)) // Some(6)

  case class Dog(name: String, yearOfBirth: Int, favoriteFoods: List[String])
  val tupleToDog: (String, Int, List[String]) => Dog = Dog
  val dogToTuple: Dog => (String, Int, List[String]) = dog => (dog.name, dog.yearOfBirth, dog.favoriteFoods)

  implicit val dogMonoid: Monoid[Dog] = (
    Monoid[String],
    Monoid[Int],
    Monoid[List[String]]
  ).imapN(tupleToDog)(dogToTuple)

  val garfield = Dog("Garfield", 1978, List("Lasagne"))
  val heathcliff = Dog("Heathcliff", 1988, List("Junk Food"))

  println(garfield |+| heathcliff) // Dog("GarfieldHeathcliff, 3966, List("Lasagne", "Junk Food"))
}
