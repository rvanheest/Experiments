package monadics.structures

import scala.language.higherKinds

trait MonadFilter[MF[_]] extends Monad[MF] {

  def empty[A]: MF[A]

  def filter[A](mf: MF[A])(predicate: A => Boolean): MF[A] = {
    flatMap(mf)(a => if (predicate(a)) create(a) else empty[A])
  }

  def filterNot[A](mf: MF[A])(predicate: A => Boolean): MF[A] = {
    filter(mf)(!predicate(_))
  }

  def collect[A, B](mf: MF[A])(pf: PartialFunction[A, B]): MF[B] = {
    flatMap(mf)(a => if (pf.isDefinedAt(a)) create(pf(a)) else empty[B])
  }
}
