package catsbook.monoid

object ExerciseBooleanAndSetMonoids {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }
  object Monoid {
    def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
  }

  val andBooleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty: Boolean = true

    def combine(x: Boolean, y: Boolean): Boolean = x && y
  }
  val orBooleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty: Boolean = false

    def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  def mergeSetMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def empty: Set[A] = Set.empty

    def combine(x: Set[A], y: Set[A]): Set[A] = x ++ y
  }
}
