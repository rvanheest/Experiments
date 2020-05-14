package monadics.laws

import monadics.structures.MonadFail

import scala.language.higherKinds

trait MonadFailLaws[M[_]] extends MonadLaws[M] {

  implicit val instance: MonadFail[M]

  // fail s >>= f == fail s
  def monadFailLeftZero[A, B](s: String, f: A => M[B]): IsEquals[M[B]] = {
    val ex = instance.fail(s)
    instance.flatMap(ex)(f) === ex.asInstanceOf[M[B]]
  }
}

object MonadFailLaws {
  def apply[M[_]](implicit mf: MonadFail[M]): MonadFailLaws[M] = new MonadFailLaws[M] {
    val instance: MonadFail[M] = mf
  }
}
