package monadics

import monadics.structures.Monoid

package object ScalaMonoids {

  implicit val stringIsMonoid: Monoid[String] = Monoid.create("")(_ + _)

  implicit class StringMonoid(val string: String)(implicit monoid: Monoid[String]) {
    def append(other: => String): String = monoid.combine(string, other)
  }
}
