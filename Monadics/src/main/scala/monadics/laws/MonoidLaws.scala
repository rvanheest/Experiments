package monadics.laws

import monadics.structures.Monoid

trait MonoidLaws[M] extends SemigroupLaws[M] {

  implicit val instance: Monoid[M]

  // mempty <> x = x
  def combineLeftIdentity(a: M): IsEquals[M] = {
    instance.combine(instance.empty, a) === a
  }

  // x <> mempty = x
  def combineRightIdentity(a: M): IsEquals[M] = {
    instance.combine(a, instance.empty) === a
  }
}

object MonoidLaws {
  def apply[M](implicit monoid: Monoid[M]): MonoidLaws[M] = new MonoidLaws[M] {
    val instance: Monoid[M] = monoid
  }
}
