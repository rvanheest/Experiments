package experiments.cats.exercises

import cats._
import cats.implicits._

object monoid extends App {

  println(Monoid[String].empty)
  println(Monoid[String].combineAll(List("a", "b", "c")))
  println(Monoid[String].combineAll(List()))
  println

  println(List("a", "b", "c").combineAll)
  println(List.empty[String].combineAll)
  println

  println(Monoid[Map[String, Int]].combineAll(List(Map("a" → 1, "b" → 2), Map("a" → 3))))
  println(Monoid[Map[String, Int]].combineAll(List()))
  println

  println(List(Map("a" → 1, "b" → 2), Map("a" → 3)).combineAll)
  println(List.empty[Map[String, Int]].combineAll)
  println

  val l = List(1, 2, 3, 4, 5)
  println(l.foldMap(x => x))
  println(l.foldMap(_.toString))
  println

  println(List(1, 2, 3, 4, 5).foldMap(i => (i, i.toString)))
}
