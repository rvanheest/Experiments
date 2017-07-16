package monadics.instances.monoids

import monadics.structures.{Equals, Monoid}

case class First[A](first: Option[A])(implicit monoid: Monoid[First[A]]) {

  def or(other: => First[A]): First[A] = {
    monoid.combine(this, other)
  }
}

object First {
  def empty[A](implicit monoid: Monoid[First[A]]): First[A] = monoid.empty

  implicit def firstIsEquals[A](implicit aEquals: Equals[Option[A]]): Equals[First[A]] = {
    Equals.create { case (First(a1), First(a2)) => aEquals.equals(a1, a2) }
  }

  implicit def firstIsMonoid[A]: Monoid[First[A]] = {
    Monoid.create(First(Option.empty[A])) {
      case (First(None), y) => y
      case (x, _) => x
    }
  }
}
