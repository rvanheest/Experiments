package monadics.instances.monoids

import monadics.structures.{Equals, Monoid}

case class Last[A](last: Option[A])(implicit monoid: Monoid[Last[A]]) {

  def or(other: => Last[A]): Last[A] = {
    monoid.combine(this, other)
  }
}

object Last {
  def empty[A](implicit monoid: Monoid[Last[A]]): Last[A] = monoid.empty

  implicit def lastIsEquals[A](implicit aEquals: Equals[Option[A]]): Equals[Last[A]] = {
    Equals.create { case (Last(a1), Last(a2)) => aEquals.equals(a1, a2) }
  }

  implicit def LastIsMonoid[A]: Monoid[Last[A]] = {
    Monoid.create(Last(Option.empty[A])) {
      case (Last(None), y) => y
      case (x, _) => x
    }
  }
}
