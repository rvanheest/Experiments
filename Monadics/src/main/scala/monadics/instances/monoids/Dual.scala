package monadics.instances.monoids

import monadics.structures.Monoid

case class Dual[A](dual: A)(implicit monoid: Monoid[Dual[A]], aMonoid: Monoid[A]) {

  def combine(other: => Dual[A]): Dual[A] = {
    monoid.combine(this, other)
  }
}

object Dual {
  def empty[A](implicit monoid: Monoid[Dual[A]], aMonoid: Monoid[A]): Dual[A] = monoid.empty

  implicit def dualIsMonoid[A](implicit aMonoid: Monoid[A]): Monoid[Dual[A]] = {
    Monoid.create(Dual(aMonoid.empty)) {
      case (Dual(x), Dual(y)) => Dual(aMonoid.combine(y, x))
    }
  }
}
