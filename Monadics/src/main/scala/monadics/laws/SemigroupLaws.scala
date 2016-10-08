package monadics.laws

import monadics.structures.Semigroup

trait SemigroupLaws[S] {

  implicit val instance: Semigroup[S]

  def associativity(a: S, b: S, c: S) = {
    instance.append(instance.append(a, b), c) == instance.append(a, instance.append(b, c))
  }
}

object SemigroupLaws {
  def apply[S](implicit sg: Semigroup[S]): SemigroupLaws[S] = new SemigroupLaws[S] {
    val instance: Semigroup[S] = sg
  }
}
