package catsbook.typeclasses

import cats.Eq
import cats.instances.int._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.option._

object EqExercise extends App {

  implicit class EqSyntax[A](val eq1: Eq[A]) extends AnyVal {
    def and(eq2: Eq[A]): Eq[A] = Eq.and(eq1, eq2)
  }

  case class Cat(name: String, age: Int, color: String)
  object Cat {
    val eqCatName: Eq[Cat] = (cat1, cat2) => cat1.name === cat2.name
    val eqCatAge: Eq[Cat] = (cat1, cat2) => cat1.age === cat2.age
    val eqCatColor: Eq[Cat] = (cat1, cat2) => cat1.color === cat2.color

    implicit val eqCat: Eq[Cat] = eqCatName and eqCatAge and eqCatColor
  }

  val cat1 = Cat("Alice", 2, "black")
  val cat2 = Cat("Bob", 3, "white")
  println(cat1 === cat2) // false
  println(cat1 =!= cat2) // true

  val maybeCat1 = cat1.some
  val maybeCat2 = none[Cat]
  println(maybeCat1 === maybeCat2) // false
  println(maybeCat1 =!= maybeCat2) // true
}
