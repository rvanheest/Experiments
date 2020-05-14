package monadics.laws

import monadics.structures.MonoidK

import scala.language.higherKinds

trait MonoidKLaws[M[_]] extends SemigroupKLaws[M] {

  implicit val instance: MonoidK[M]

  // empty <|> x = x
  def combineLeftIdentity[A](ma: M[A]): IsEquals[M[A]] = {
    instance.combine(instance.empty[A], ma) === ma
  }

  // x <|> empty = x
  def combineRightIdentity[A](ma: M[A]): IsEquals[M[A]] = {
    instance.combine(ma, instance.empty[A]) === ma
  }
}

object MonoidKLaws {
  def apply[S[_]](implicit mk: MonoidK[S]): MonoidKLaws[S] = new MonoidKLaws[S] {
    override implicit val instance = mk
  }
}
