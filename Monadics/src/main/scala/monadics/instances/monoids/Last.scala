package monadics.instances.monoids

import monadics.structures.Monoid

case class Last[A](last: Option[A])(implicit monoid: Monoid[Last[A]]) {

  def or(other: => Last[A]): Last[A] = {
    monoid.combine(this, other)
  }
}

object Last {
  def empty[A](implicit monoid: Monoid[Last[A]]): Last[A] = monoid.empty

  implicit def LastIsMonoid[A]: Monoid[Last[A]] = {
    Monoid.create(Last(Option.empty[A])) {
      case (Last(None), y) => y
      case (x, _) => x
    }
  }
}
