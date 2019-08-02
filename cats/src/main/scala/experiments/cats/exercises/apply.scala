package experiments.cats.exercises

import cats._
import cats.implicits._

object apply extends App {

  val intToString: Int => String = _.toString
  val double: Int => Int = _ * 2
  val addTwo: Int => Int = _ + 2

  println(Apply[Option].map(Some(1))(intToString))
  println(Apply[Option].map(Some(1))(double))
  println(Apply[Option].map(None)(addTwo))
  println

  val listOpt = Apply[List] compose Apply[Option]
  val plusOne = (x: Int) => x + 1
  println(listOpt.ap(List(Some(plusOne)))(List(Some(1), None, Some(3))))
  println

  println(Apply[Option].ap(Some(intToString))(Some(1)))
  println(Apply[Option].ap(Some(double))(Some(1)))
  println(Apply[Option].ap(Some(double))(None))
  println(Apply[Option].ap(None)(Some(1)))
  println(Apply[Option].ap(None)(None))
  println

  val addArity2 = (a: Int, b: Int) => a + b
  println(Apply[Option].ap2(Some(addArity2))(Some(1), Some(2)))
  println(Apply[Option].ap2(Some(addArity2))(Some(1), None))
  println

  val addArity3 = (a: Int, b: Int, c: Int) => a + b + c
  println(Apply[Option].ap3(Some(addArity3))(Some(1), Some(2), Some(3)))
  println

  println(Apply[Option].map2(Some(1), Some(2))(addArity2))
  println(Apply[Option].map3(Some(1), Some(2), Some(3))(addArity3))
  println

  println(Apply[Option].tuple2(Some(1), Some(2)))
  println(Apply[Option].tuple3(Some(1), Some(2), Some(3)))
  println

  val option2 = Option(1) |@| Option(2)
  val option3 = option2 |@| Option.empty[Int]

  println(option2 map addArity2)
  println(option3 map addArity3)
  println

  println(option2 apWith Some(addArity2))
  println(option3 apWith Some(addArity3))
  println

  println(option2.tupled)
  println(option3.tupled)
}
