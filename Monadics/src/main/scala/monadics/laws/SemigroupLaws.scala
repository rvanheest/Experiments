package monadics.laws

import monadics.structures.Semigroup

trait SemigroupLaws[S] {

  implicit val instance: Semigroup[S]

  // (x <> y) <> z = x <> (y <> z)
  def associativity(a: S, b: S, c: S): IsEquals[S] = {
    instance.combine(instance.combine(a, b), c) === instance.combine(a, instance.combine(b, c))
  }
}

object SemigroupLaws {
  def apply[S](implicit sg: Semigroup[S]): SemigroupLaws[S] = new SemigroupLaws[S] {
    val instance: Semigroup[S] = sg
  }
}
