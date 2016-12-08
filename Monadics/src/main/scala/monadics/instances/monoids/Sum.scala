package monadics.instances.monoids

import monadics.structures.{Equals, Monoid}

case class Sum[A](sum: A)(implicit numeric: Numeric[A], monoid: Monoid[Sum[A]]) {

  def +(other: => Sum[A]): Sum[A] = {
    monoid.combine(this, other)
  }
}

object Sum {
  def empty[A](implicit numeric: Numeric[A], monoid: Monoid[Sum[A]]): Sum[A] = {
    monoid.empty
  }

  implicit def sumIsEquals[A](implicit aEquals: Equals[A]): Equals[Sum[A]] = {
    Equals.create { case (Sum(a1), Sum(a2)) => aEquals.equals(a1, a2) }
  }

  implicit def sumIsMonoid[A](implicit numeric: Numeric[A]): Monoid[Sum[A]] = {
    Monoid.create(Sum(numeric.zero)) {
      case (Sum(a1), Sum(a2)) => Sum(numeric.plus(a1, a2))
    }
  }
}
