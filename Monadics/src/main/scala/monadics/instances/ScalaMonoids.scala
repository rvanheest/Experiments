package monadics

import monadics.structures.{Monoid, Semigroup}

package object ScalaMonoids {

  implicit def stringIsMonoid: Monoid[String] = new Monoid[String] {
    def empty: String = ""

    def append(a1: String, a2: => String): String = a1 + a2
  }

  implicit class StringMonoid(val string: String)(implicit monoid: Monoid[String]) {
    def append(other: => String): String = monoid.append(string, other)
  }

  implicit def listIsMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def empty: List[A] = Nil

    def append(a1: List[A], a2: => List[A]): List[A] = a1 ++ a2
  }

  implicit def optionOfSemigroupIsMonoid[A](implicit monoid: Semigroup[A]): Monoid[Option[A]] = new Monoid[Option[A]] {
    def empty: Option[A] = Option.empty

    def append(optA: Option[A], optB: => Option[A]): Option[A] = {
      optA.map(a =>
        optB.map(b => Some(monoid.append(a, b)))
          .getOrElse(optA))
        .getOrElse(optB)
    }
  }

  implicit class OptionMonoid[A: Semigroup](val option: Option[A])(implicit monoid: Monoid[Option[A]]) {
    def append(other: => Option[A]): Option[A] = monoid.append(option, other)
  }
}
