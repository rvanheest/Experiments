package monadics.instances.monoids

import monadics.structures.Monoid

case class First[A](first: Option[A])(implicit monoid: Monoid[First[A]]) {

  def or(other: => First[A]): First[A] = {
    monoid.combine(this, other)
  }
}

object First {
  def empty[A](implicit monoid: Monoid[First[A]]): First[A] = monoid.empty

  implicit def firstIsMonoid[A]: Monoid[First[A]] = {
    Monoid.create(First(Option.empty[A])) {
      case (First(None), y) => y
      case (x, _) => x
    }
  }
}
