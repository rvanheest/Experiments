package monadics.structures

import scala.language.higherKinds

trait MonadFilter[MF[_]] extends Monad[MF] {

  def empty[A]: MF[A]

  def filter[A](mf: MF[A])(predicate: A => Boolean): MF[A] = {
    flatMap(mf)(a => if (predicate(a)) create(a) else empty[A])
  }

  def filterNot[A](mp: MF[A])(predicate: A => Boolean): MF[A] = {
    filter(mp)(!predicate(_))
  }
}
