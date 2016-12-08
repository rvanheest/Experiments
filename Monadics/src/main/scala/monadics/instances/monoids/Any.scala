package monadics.instances.monoids

import monadics.structures.{Equals, Monoid}

case class Any(any: Boolean)(implicit monoid: Monoid[Any]) {

  def combine(other: => Any): Any = {
    monoid.combine(this, other)
  }
}

object Any {
  def empty(implicit monoid: Monoid[Any]): Any = monoid.empty

  implicit def anyIsEquals(implicit aEquals: Equals[Boolean]): Equals[Any] = {
    Equals.create { case (Any(x), Any(y)) => aEquals.equals(x, y) }
  }

  implicit def anyIsMonoid: Monoid[Any] = {
    Monoid.create(Any(any = false)) {
      case (Any(x), Any(y)) => Any(x || y)
    }
  }
}
