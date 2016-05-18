package experiments.monadics.instances

import experiments.monadics.MonadPlus

sealed abstract class Maybe[+A](implicit m: MonadPlus[Maybe]) {

  def map[B](f: A => B): Maybe[B] = m.map(this)(f)

  def <*>[B, C](other: Maybe[B])(implicit ev: A <:< (B => C)): Maybe[C] = {
    m.<*>(this.map(ev), other)
  }

  def <**>[B](other: Maybe[A => B]): Maybe[B] = m.<**>(this, other)

  def flatMap[B](f: A => Maybe[B]): Maybe[B] = m.flatMap(this)(f)
  def >>=[B](f: A => Maybe[B]): Maybe[B] = flatMap(f)

  def flatten[B](implicit ev: A <:< Maybe[B]): Maybe[B] = m.flatten(this)(ev)

  def getOrElse[B >: A](default: => B): B = m.getOrElse(this, default)

  def orElse[B >: A](other: => Maybe[B]): Maybe[B] = m.orElse(this, other)

  def maybe = m.maybe(this)

  def filter(predicate: A => Boolean): Maybe[A] = m.filter(this)(predicate)

  def filterNot(predicate: A => Boolean): Maybe[A] = m.filterNot(this)(predicate)

  def ifPresent[U](f: A => U): Unit

  def doOnDefined(f: A => Unit): Maybe[A]

  def doOnEmpty(f: () => Unit): Maybe[A]
}

object Maybe {
  def apply[A](a: A): Maybe[A] = Just(a)
  def empty[A]: Maybe[A] = None
}

case class Just[+A](a: A) extends Maybe[A] {
  def ifPresent[U](f: A => U) = f(a)

  def doOnDefined(f: A => Unit): Maybe[A] = {
    f(a)
    this
  }

  def doOnEmpty(f: () => Unit): Maybe[A] = this
}

case object None extends Maybe[Nothing] {
  def ifPresent[U](f: Nothing => U) = ()

  def doOnDefined(f: Nothing => Unit): Maybe[Nothing] = this

  def doOnEmpty(f: () => Unit): Maybe[Nothing] = {
    f()
    this
  }
}
