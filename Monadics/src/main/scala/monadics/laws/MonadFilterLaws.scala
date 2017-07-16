package monadics.laws

import monadics.structures.MonadFilter

import scala.language.higherKinds

trait MonadFilterLaws[MF[_]] extends MonadLaws[MF] {

  implicit val instance: MonadFilter[MF]

  // empty >>= f == empty
  def monadFilterLeftDistributivity[A, B](f: A => MF[B]): IsEquals[MF[B]] = {
    instance.flatMap(instance.empty[A])(f) === instance.empty[B]
  }

  // m >>= (\_ -> empty) == empty
  def monadFilterRightDistributivity[A](mfa: MF[A]): IsEquals[MF[A]] = {
    instance.flatMap(mfa)(_ => instance.empty[A]) === instance.empty[A]
  }
}

object MonadFilterLaws {
  def apply[MF[_]](implicit mf: MonadFilter[MF]): MonadFilterLaws[MF] = new MonadFilterLaws[MF] {
    val instance: MonadFilter[MF] = mf
  }
}
