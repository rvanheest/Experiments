package monadics.instances.monoids

import monadics.structures.{Equals, Monoid}

case class Dual[A](dual: A)(implicit monoid: Monoid[Dual[A]]) {

  def combine(other: => Dual[A]): Dual[A] = monoid.combine(this, other)
}

object Dual {
  def empty[A](implicit monoid: Monoid[Dual[A]], aMonoid: Monoid[A]): Dual[A] = monoid.empty

  implicit def dualIsEquals[A](implicit aEquals: Equals[A]): Equals[Dual[A]] = {
    Equals.create { case (Dual(a1), Dual(a2)) => aEquals.equals(a1, a2) }
  }

  implicit def dualIsMonoid[A](implicit aMonoid: Monoid[A]): Monoid[Dual[A]] = {
    Monoid.create(Dual(aMonoid.empty)) {
      case (Dual(a1), Dual(a2)) => Dual(aMonoid.combine(a2, a1))
    }
  }
}
