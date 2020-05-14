package catsbook.monoid

import cats.Monoid
import cats.instances.int._
import cats.instances.option._
import cats.syntax.option._

object ExerciseAddingAllTheThings extends App {

  def add[A: Monoid](items: List[A]): A = Monoid[A].combineAll(items)

  println(add(List.empty[Int])) // 0
  println(add(1 :: 2 :: 3 :: Nil)) // 6

  println(add(List.empty[Int])) // None
  println(add(none[Int] :: Nil)) // None
  println(add(1.some :: none :: 3.some :: Nil)) // Some(4)

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    def empty: Order = Order(0, 0)

    def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

  println(add(Order(1.1, 3) :: Order(12, 4) :: Nil)) // Order(13.1, 7)
}
