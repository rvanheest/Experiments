package catsbook.typeclasses

import java.util.Date

import cats.Eq
import cats.instances.int._
import cats.instances.long._
import cats.instances.option._
import cats.syntax.eq._
import cats.syntax.option._

object EqTypeClass extends App {

  val eqInt = Eq[Int]
  println(eqInt.eqv(123, 123)) // true
  println(eqInt.eqv(123, 456)) // false
//  eqInt.eqv(123, "123") --> doesn't compile since 123 has a different type than "123"

  println(123 === 123) // true
  println(123 =!= 456) // true
//  123 === "123" --> doesn't compile since 123 has a different type than "123"

  println(1.some === none[Int]) // false
  println(1.some =!= none[Int]) // true

  implicit val dateEq: Eq[Date] = _.getTime === _.getTime

  val date1 = new Date()
  Thread.sleep(10) // ms
  val date2 = new Date()

  println(date1 === date1) // true
  println(date1 === date2) // false
}
