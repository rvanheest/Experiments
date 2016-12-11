package monadics.structures

import scala.language.higherKinds

trait MonoidK[M[_]] extends SemigroupK[M] {
  def empty[A]: M[A]

  def toMonoid[A]: Monoid[M[A]] = {
    val self: MonoidK[M] = this
    new Monoid[M[A]] {
      override def empty: M[A] = self.empty[A]

      override def combine(x: M[A], y: => M[A]): M[A] = self.combine(x, y)
    }
  }
}
