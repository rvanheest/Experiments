package experiments.cats.exercises

import cats._
import cats.implicits._

object identity extends App {

  val x: Id[Int] = 1
  val y: Int = x
  println(y)

  val one: Id[Int] = 1
  val fourtytwo: Id[Int] = 42

  println(Functor[Id].map(one)(_ + 1))
  println(one.map(_ + 1))
  println

  println(Applicative[Id].pure(fourtytwo))
  println

  println(Monad[Id].map(one)(_ + 1))
  println(one.map(_ + 1))
  println

  println(Monad[Id].flatMap(one)(_ + 1))
  println(one.flatMap(_ + 1))
  println

  println(Comonad[Id].coflatMap(42)(_ + 1))
  println(fourtytwo.coflatMap(_ + 1))
}
