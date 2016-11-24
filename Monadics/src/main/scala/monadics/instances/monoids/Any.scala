package monadics.instances.monoids

import monadics.structures.Monoid

case class Any(any: Boolean)(implicit monoid: Monoid[Any]) {

  def combine(other: => Any): Any = {
    monoid.combine(this, other)
  }
}

object Any {
  def empty(implicit monoid: Monoid[Any]): Any = monoid.empty

  implicit def anyIsMonoid: Monoid[Any] = {
    Monoid.create(Any(any = false))((x, y) => Any(x.any || y.any))
  }
}
