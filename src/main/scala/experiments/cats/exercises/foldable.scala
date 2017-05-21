package experiments.cats.exercises

import cats._
import cats.implicits._

object foldable extends App {

  println(Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _))
  println(Foldable[List].foldLeft(List("a", "b", "c"), "")(_ + _))
  println

  val lazyResult = Foldable[List].foldRight(List(1, 2, 3), Now(0))((x, rest) ⇒ Later(x + rest.value))
  println(lazyResult.value)
  println

  println(Foldable[List].fold(List("a", "b", "c")))
  println(Foldable[List].fold(List(1, 2, 3)))
  println

  println(Foldable[List].foldMap(List("a", "b", "c"))(_.length))
  println(Foldable[List].foldMap(List(1, 2, 3))(_.toString))
  println

  println(List("a", "b", "c").foldMap(_.length))
  println(List(1, 2, 3).foldMap(_.toString))
  println

  println(Foldable[List].foldK(List(List(1, 2), List(3, 4, 5))))
  println(Foldable[List].foldK(List(None, Option("two"), Option("three"))))
  println

  println(List(List(1, 2), List(3, 4, 5)).foldK)
  println(List(None, Option("two"), Option("three")).foldK)
  println

  println(Foldable[List].find(List(1, 2, 3))(_ > 2))
  println(Foldable[List].find(List(1, 2, 3))(_ > 5))
  println

  println(List(1, 2, 3).find(_ > 2))
  println(List(1, 2, 3).find(_ > 5))
  println

  println(Foldable[List].exists(List(1, 2, 3))(_ > 2))
  println(Foldable[List].exists(List(1, 2, 3))(_ > 5))
  println

  println(List(1, 2, 3).exists(_ > 2))
  println(List(1, 2, 3).exists(_ > 5))
  println

  println(Foldable[List].forall(List(1, 2, 3))(_ <= 3))
  println(Foldable[List].forall(List(1, 2, 3))(_ < 3))
  println

  println(List(1, 2, 3).forall(_ <= 3))
  println(List(1, 2, 3).forall(_ < 3))
  println

  println(Foldable[List].toList(List(1, 2, 3)))
  println(Foldable[Option].toList(Option(42)))
  println(Foldable[Option].toList(None))
  println

  println(List(1, 2, 3).toList)
  println(Option(42).toList)
  println(None.toList)
  println

  println(Foldable[List].filter_(List(1, 2, 3))(_ < 3))
  println(Foldable[Option].filter_(Option(42))(_ != 42))
  println

  println(List(1, 2, 3).filter_(_ < 3))
  println(Option(42).filter_(_ != 42))
  println

  def parseInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption
  println(Foldable[List].traverse_(List("1", "2", "3"))(parseInt))
  println(Foldable[List].traverse_(List("a", "b", "c"))(parseInt))
  println

  println(List("1", "2", "3").traverse_(parseInt))
  println(List("a", "b", "c").traverse_(parseInt))
  println

  val FoldableListOption = Foldable[List].compose[Option]
  println(FoldableListOption.fold(List(Option(1), Option(2), Option(3), Option(4))))
  println(FoldableListOption.fold(List(Option("1"), Option("2"), None, Option("3"))))
  println

  println(Foldable[List].isEmpty(List(1, 2, 3)))
  println(Foldable[List].dropWhile_(List(1, 2, 3))(_ < 2))
  println(Foldable[List].takeWhile_(List(1, 2, 3))(_ < 2))
  println

  println(List(1, 2, 3).isEmpty)
  println(List(1, 2, 3).dropWhile_(_ < 2))
  println(List(1, 2, 3).takeWhile_(_ < 2))
}
