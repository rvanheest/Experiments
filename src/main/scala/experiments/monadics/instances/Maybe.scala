package experiments.monadics.instances

import experiments.monadics.MonadPlus

sealed abstract class Maybe[+A](implicit m: MonadPlus[Maybe]) {
  
  def map[B](f: A => B): Maybe[B] = m.map(f, this)

  def <*>[B, C](other: Maybe[B])(implicit ev: A <:< (B => C)): Maybe[C] = {
    m.<*>(this.map(ev), other)
  }

  def <**>[B](other: Maybe[A => B]): Maybe[B] = m.<**>(this, other)

  def flatMap[B](f: A => Maybe[B]): Maybe[B] = m.flatMap(this, f)
  def >>=[B](f: A => Maybe[B]): Maybe[B] = flatMap(f)

  def andThen[B](other: Maybe[B]): Maybe[B] = m.andThen(this, other)
  def >>[B](other: Maybe[B]): Maybe[B] = andThen(other)

  def thenAnd[B](other: Maybe[B]): Maybe[A] = m.thenAnd(this, other)
  def <<[B](other: Maybe[B]): Maybe[A] = thenAnd(other)

  def flatten[B](implicit ev: A <:< Maybe[B]): Maybe[B] = m.flatten(this)(ev)

  def getOrElse[B >: A](default: => B): B = m.getOrElse(this, default)

  def orElse[B >: A](other: => Maybe[B]): Maybe[B] = m.orElse(this, other)

  def some = m.some(this)

  def many = m.many(this)

  def maybe = m.maybe(this)

  def filter(predicate: A => Boolean): Maybe[A] = {
    flatMap(a => if (predicate(a)) m.create(a) else m.empty)
  }
}
object Maybe {
  def apply[A](a: A): Maybe[A] = Just(a)
  def empty[A]: Maybe[A] = None
}

case class Just[+A](a: A) extends Maybe[A]
case object None extends Maybe[Nothing]
