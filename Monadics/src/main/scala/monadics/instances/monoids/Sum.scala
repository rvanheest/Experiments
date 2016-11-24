package monadics.instances.monoids

import monadics.structures.Monoid

case class Sum[A](sum: A)(implicit numeric: Numeric[A], monoid: Monoid[Sum[A]]) {

  def +(other: => Sum[A]): Sum[A] = {
    monoid.combine(this, other)
  }
}

object Sum {
  def empty[A](implicit numeric: Numeric[A], monoid: Monoid[Sum[A]]): Sum[A] = {
    monoid.empty
  }

  implicit def sumIsMonoid[A](implicit numeric: Numeric[A]): Monoid[Sum[A]] = new Monoid[Sum[A]] { self =>
    implicit val selfMonoid: Monoid[Sum[A]] = self

    def empty: Sum[A] = {
      Sum(numeric.zero)
    }

    def combine(a1: Sum[A], a2: => Sum[A]): Sum[A] = {
      Sum(numeric.plus(a1.sum, a2.sum))
    }
  }
}
