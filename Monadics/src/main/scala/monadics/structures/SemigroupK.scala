package monadics.structures

import scala.language.higherKinds

trait SemigroupK[S[_]] {
  def combine[A, B >: A](x: S[A], y: => S[B]): S[B]

  def toSemigroup[A]: Semigroup[S[A]] = {
    val self = this
    new Semigroup[S[A]] {
      override def combine(x: S[A], y: => S[A]) = self.combine(x, y)
    }
  }
}
