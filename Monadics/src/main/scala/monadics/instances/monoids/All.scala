package monadics.instances.monoids

import monadics.structures.Monoid

case class All(any: Boolean)(implicit monoid: Monoid[All]) {

  def combine(other: => All): All = {
    monoid.combine(this, other)
  }
}

object All {
  def empty(implicit monoid: Monoid[All]): All = monoid.empty

  implicit def anyIsMonoid: Monoid[All] = {
    Monoid.create(All(any = true))((x, y) => All(x.any && y.any))
  }
}
