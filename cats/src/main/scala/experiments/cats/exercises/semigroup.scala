package experiments.cats.exercises

import cats.Semigroup
import cats.implicits._

import scala.language.postfixOps

object semigroup extends App {

  println(Semigroup[Int].combine(1, 2))
  println(Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)))
  println(Semigroup[Option[Int]].combine(Option(1), Option(2)))
  println(Semigroup[Option[Int]].combine(Option(1), None))
  println(Semigroup[Int => Int].combine(1 +, 10 *)(6))
  println

  println(1.combine(2))
  println(List(1, 2, 3).combine(List(4, 5, 6)))
  println(Option(1).combine(Option(2)))
  println(Option(1).combine(None))
  println(((x: Int) => x + 1).combine(10 *).apply(6))
  println

  println(Map("foo" -> Map("bar" -> 5)).combine(Map("foo" -> Map("bar" -> 6), "baz" -> Map())))
  println(Map("foo" -> List(1, 2)).combine(Map("foo" -> List(3, 4), "bar" -> List(42))))
  println(Map("foo" -> Map("bar" -> 5)) ++ Map("foo" -> Map("bar" -> 6), "baz" -> Map()))
  println(Map("foo" -> List(1, 2)) ++ Map("foo" -> List(3, 4), "bar" -> List(42)))
  println

  val aMap = Map("foo" -> Map("bar" -> 5))
  val anotherMap = Map("foo" -> Map("bar" -> 6))
  val combinedMap = Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)
  println(combinedMap.get("foo"))
  println

  val one: Option[Int] = Option(1)
  val two: Option[Int] = Option(2)
  val n: Option[Int] = None
  println(one |+| two)
  println(n |+| two)
  println(n |+| n)
  println(two |+| n)
}
