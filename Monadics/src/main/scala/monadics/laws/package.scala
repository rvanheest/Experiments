package monadics

import monadics.structures.Equals

package object laws {

  case class IsEquals[A](x: A, y: A) {

    def isEqual(implicit equal: Equals[A]): Boolean = equal.equals(x, y)
  }

  implicit class InfixEquals[A](val x: A) extends AnyVal {
    def ===(y: A): IsEquals[A] = IsEquals(x, y)
  }
}
