package experiments.view

import scala.language.implicitConversions

sealed abstract class BackwardsListView
object BackwardsListView extends View[List[Int], BackwardsListView] {

  case object EmptyList extends BackwardsListView
  case class Snoc(list: List[Int], last: Int) extends BackwardsListView

  /**
   * {{{
   * in (x Cons Nil)          = Nil Snoc x
   * in (x Cons (xs Snoc x')) = (x Cons xs) Snoc x'
   * }}}
   */
  implicit def in(list: List[Int]): BackwardsListView = list match {
    case Nil => EmptyList
    case x :: xs => in(xs) match {
      case EmptyList => Snoc(Nil, x)
      case Snoc(ys, y) => Snoc(x :: ys, y)
    }
  }

  /**
   * {{{
   * out (Nil Snoc x)          = x Cons Nil
   * out ((x Cons xs) Snoc x') = x Cons (xs Snoc x')
   * }}}
   */
  implicit def out(backwards: BackwardsListView): List[Int] = backwards match {
    case EmptyList => Nil
    case Snoc(Nil, y) => y :: Nil
    case Snoc(x :: xs, y) => x :: out(Snoc(xs, y))
  }
}

object BackwardsListDemo extends App {

  import BackwardsListView._

  println(in(Nil))
  println(in(List(1)))
  println(in(List(1, 2)))
  println(in(List(1, 2, 3)))

  println(out(in(Nil)))
  println(out(in(List(1))))
  println(out(in(List(1, 2))))
  println(out(in(List(1, 2, 3))))

  println(out(Snoc(Snoc(Nil, 1), 2)))

  /**
   * {{{last (xs Snoc x) = x}}}
   */
  def last(list: List[Int]): Int =
    implicitly[BackwardsListView](list) match {
      case EmptyList => throw new NoSuchElementException("empty list")
      case Snoc(_, x) => x
    }

  /**
   * {{{rotateLeft (x Cons xs) = xs Snoc x}}}
   */
  def rotateLeft(list: List[Int]): List[Int] =
    list match {
      case Nil => EmptyList
      case x :: xs => Snoc(xs, x)
    }

  /**
   * {{{rotateRight (xs Snoc x) = x Cons xs}}}
   */
  def rotateRight(list: List[Int]): List[Int] =
    implicitly[BackwardsListView](list) match {
      case EmptyList => Nil
      case Snoc(xs, x) => x :: xs
    }

  println(last(List(1, 2, 3)))
  println(rotateLeft(List(1, 2, 3)))
  println(rotateRight(List(1, 2, 3)))
}
