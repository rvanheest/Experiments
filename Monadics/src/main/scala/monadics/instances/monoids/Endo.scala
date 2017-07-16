package monadics.instances.monoids

import monadics.structures.{Equals, Monoid}

case class Endo[A](endo: A => A)(implicit monoid: Monoid[Endo[A]]) {

  def combine(other: => Endo[A]): Endo[A] = {
    monoid.combine(this, other)
  }
}

object Endo {
  def empty[A](implicit monoid: Monoid[Endo[A]]): Endo[A] = monoid.empty

  implicit def endoIsEquals[A](implicit fEquals: Equals[A => A]): Equals[Endo[A]] = {
    Equals.create { case (Endo(e1), Endo(e2)) => fEquals.equals(e1, e2) }
  }

  implicit def endoIsMonoid[A]: Monoid[Endo[A]] = {
    Monoid.create(Endo(identity[A])) {
      case (Endo(f), Endo(g)) => Endo(f compose g)
    }
  }
}
