package monadics.laws

import monadics.structures.Monoid

trait MonoidLaws[M] extends SemigroupLaws[M] {

  implicit val instance: Monoid[M]

  def combineRightIdentity(a: M) = {
    instance.append(a, instance.empty) == a
  }

  def combineLeftIdentity(a: M) = {
    instance.append(instance.empty, a) == a
  }
}

object MonoidLaws {
  def apply[M](implicit monoid: Monoid[M]): MonoidLaws[M] = new MonoidLaws[M] {
    val instance: Monoid[M] = monoid
  }
}
