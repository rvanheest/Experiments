package experiments.monadics.instances

import experiments.monadics.{MonadPlus, Semigroup}

sealed abstract class Maybe[+A](implicit mp: MonadPlus[Maybe]) {

  def append[B >: A](other: Maybe[B])(implicit semigroup: Semigroup[Maybe[B]]): Maybe[B] = {
    semigroup.append(this, other)
  }

  def map[B](f: A => B): Maybe[B] = mp.map(this)(f)

  def <*>[B, C](other: Maybe[B])(implicit ev: A <:< (B => C)): Maybe[C] = {
    mp.<*>(this.map(ev), other)
  }

  def <**>[B](other: Maybe[A => B]): Maybe[B] = mp.<**>(this, other)

  def flatMap[B](f: A => Maybe[B]): Maybe[B] = mp.flatMap(this)(f)
  def >>=[B](f: A => Maybe[B]): Maybe[B] = flatMap(f)

  def flatten[B](implicit ev: A <:< Maybe[B]): Maybe[B] = mp.flatten(this)(ev)

  def getOrElse[B >: A](default: => B): B = mp.getOrElse(this, default)

  def orElse[B >: A](other: => Maybe[B]): Maybe[B] = mp.orElse(this, other)

  def maybe: Maybe[Maybe[A]] = mp.maybe(this)

  def filter(predicate: A => Boolean): Maybe[A] = mp.filter(this)(predicate)

  def filterNot(predicate: A => Boolean): Maybe[A] = mp.filterNot(this)(predicate)

  def ifPresent[U](f: A => U): Unit

  def doOnDefined(f: A => Unit): Maybe[A]

  def doOnEmpty(f: () => Unit): Maybe[A]
}

object Maybe {
  def apply[A](a: A): Maybe[A] = Just(a)
  def empty[A]: Maybe[A] = None
}

case class Just[+A](a: A) extends Maybe[A] {
  def ifPresent[U](f: A => U): Unit = f(a)

  def doOnDefined(f: A => Unit): Maybe[A] = {
    f(a)
    this
  }

  def doOnEmpty(f: () => Unit): Maybe[A] = this
}

case object None extends Maybe[Nothing] {
  def ifPresent[U](f: Nothing => U): Unit = ()

  def doOnDefined(f: Nothing => Unit): Maybe[Nothing] = this

  def doOnEmpty(f: () => Unit): Maybe[Nothing] = {
    f()
    this
  }
}
