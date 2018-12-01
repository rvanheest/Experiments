package catsbook.semigroupal

import cats.Semigroupal
import cats.instances.future._
import cats.syntax.apply._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.language.postfixOps

object SemigroupalFuture extends App {

  val futurePair = Semigroupal[Future].product(Future("hello"), Future(123))

  println(Await.result(futurePair, 10 seconds)) // ("hello", 123)

  case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])

  val futureCat = (
    Future("Garfield"),
    Future(1978),
    Future(List("Lasagne")),
  ).mapN(Cat)

  println(Await.result(futureCat, 10 seconds)) // Cat("Garfield", 1978, List("Lasagne"))
}
