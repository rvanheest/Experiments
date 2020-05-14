package catsbook.functor

import cats.Functor
import cats.instances.list._
import cats.instances.option._
import cats.syntax.functor._

object CatsFunctor extends App {

  val list1 = List(1, 2, 3)
  val list2 = Functor[List].map(list1)(_ * 2)
  println(list2) // [2, 4, 6]

  val option1 = Option(123)
  val option2 = Functor[Option].map(option1)(_.toString)
  println(option2) // Some("123")

  val func: Int => Int = x => x + 1
  val liftedFunc = Functor[Option].lift(func)
  println(liftedFunc(Option(1))) // Some(2)

  def doMath[F[_]: Functor](start: F[Int]): F[Int] = start.map(n => n + 1 * 2)

  println(doMath(Option(20))) // Some(22)
  println(doMath(List(1, 2, 3))) // [3, 4, 5]

  case class Box[A](value: A)

  implicit val boxFunctor: Functor[Box] = new Functor[Box] {
    override def map[A, B](box: Box[A])(f: A => B): Box[B] = Box(f(box.value))
  }

  val box = Box(123)
  println(box.map(value => value + 1)) // Box(124)


}
