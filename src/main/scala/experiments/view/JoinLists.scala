package experiments.view

import scala.language.implicitConversions

sealed abstract class JoinListView
object JoinListsView extends View[JoinListView, List[Int]] {

  case object Empty extends JoinListView
  case class Unit(value: Int) extends JoinListView
  case class Join(left: JoinListView, right: JoinListView) extends JoinListView

  implicit def in(join: JoinListView): List[Int] = join match {
    case Empty => Nil
    case Unit(x) => x :: Nil
    case Join(l, r) => (l, r) match {
      case (Empty, xs) => in(xs)
      case (Unit(x), xs) => x :: in(xs)
      case (Join(xs, ys), zs) => in(Join(xs, Join(ys, zs)))
    }
  }

  implicit def out(list: List[Int]): JoinListView = list match {
    case Nil => Empty
    case x :: Nil => Join(Unit(x), Empty)
    case x :: xs => Join(Unit(x), out(xs))
  }
}

object JoinListViewDemo extends App {

  import JoinListsView._

  println(out(Nil))
  println(out(List(1)))
  println(out(List(1, 2)))
  println(out(List(1, 2, 3)))

  println(in(out(Nil)))
  println(in(out(List(1))))
  println(in(out(List(1, 2))))
  println(in(out(List(1, 2, 3))))

  println(in(Join(Unit(1), Unit(2))))
  println(in(Join(Join(Empty, Unit(1)), Join(Unit(2), Empty))))
  println(in(Join(Unit(1), Join(Unit(2), Empty))))

  println(out(in(Join(Unit(1), Unit(2)))))
  println(out(in(Join(Join(Empty, Unit(1)), Join(Unit(2), Empty)))))
  println(out(in(Join(Unit(1), Join(Unit(2), Empty)))))


}
