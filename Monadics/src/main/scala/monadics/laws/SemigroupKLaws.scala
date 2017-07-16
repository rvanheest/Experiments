package monadics.laws

import monadics.structures.SemigroupK

import scala.language.higherKinds

trait SemigroupKLaws[S[_]] {

  implicit val instance: SemigroupK[S]

  // (x <|> y) <|> z = x <|> (y <|> z)
  def associativity[A](a: S[A], b: S[A], c: S[A]): IsEquals[S[A]] = {
    instance.combine(instance.combine(a, b), c) === instance.combine(a, instance.combine(b, c))
  }
}

object SemigroupKLaws {
  def apply[S[_]](implicit sgk: SemigroupK[S]): SemigroupKLaws[S] = new SemigroupKLaws[S] {
    override implicit val instance = sgk
  }
}
