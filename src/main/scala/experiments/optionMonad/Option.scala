package experiments.optionMonad

import scala.language.implicitConversions

sealed trait Option[+A] {

  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](default: => Option[B]): Option[B]

  def foreach[U](f: A => U): Unit

  def flatMap[B](f: A => Option[B]): Option[B]

  def flatten[B](implicit ev: A <:< Option[B]): Option[B]

  def map[B](f: A => B): Option[B]

  def filter(p: A => Boolean): Option[A]

  def filterNot(p: A => Boolean): Option[A]

  def collect[B](pf: PartialFunction[A, B]): Option[B]

  def exist(p: A => Boolean): Boolean

  def forall(p: A => Boolean): Boolean

  def contains[B >: A](elem: B): Boolean

  def doOnDefined(f: A => Unit): Option[A]

  def doOnEmpty(f: () => Unit): Option[A]
}

object Option {

  implicit def option2Iterable[A](xo: Option[A]): Iterable[A] = {
    xo.map(List(_)).getOrElse(Nil)
  }

  implicit def old2NewOption[A](xo: scala.Option[A]): Option[A] = {
    xo.map(Option(_)).getOrElse(Option.empty)
  }

  def apply[A](x: A): Option[A] = {
    if (x == null)
      None()
    else
      Some(x)
  }

  def empty[A]: Option[A] = None()
}

private final case class Some[+A](value: A) extends Option[A] {

  def getOrElse[B >: A](default: => B): B = value

  def orElse[B >: A](default: => Option[B]): Option[B] = this

  def foreach[U](f: A => U): Unit = f(value)

  def flatMap[B](f: A => Option[B]): Option[B] = f(value)

  def flatten[B](implicit ev: A <:< Option[B]): Option[B] = {
    ev(value)
  }

  def map[B](f: A => B): Option[B] = Some(f(value))

  def filter(p: A => Boolean): Option[A] = {
    if (p(value))
      this
    else
      None()
  }

  def filterNot(p: A => Boolean): Option[A] = {
    if (!p(value))
      this
    else
      None()
  }

  def collect[B](pf: PartialFunction[A, B]): Option[B] = {
    pf.lift(value)
  }

  def exist(p: A => Boolean): Boolean = p(value)

  def forall(p: A => Boolean): Boolean = p(value)

  def contains[B >: A](elem: B): Boolean = value == elem

  def doOnDefined(f: A => Unit): Option[A] = {
    f(value)
    this
  }

  def doOnEmpty(f: () => Unit): Option[A] = this
}

private final case class None[A]() extends Option[A] {

  def getOrElse[B >: A](default: => B): B = default

  def orElse[B >: A](default: => Option[B]): Option[B] = default

  def foreach[U](f: A => U): Unit = ()

  def flatMap[B](f: A => Option[B]): Option[B] = None()

  def flatten[B](implicit ev: A <:< Option[B]): Option[B] = None()

  def map[B](f: A => B): Option[B] = None()

  def filter(p: A => Boolean): Option[A] = None()

  def filterNot(p: A => Boolean): Option[A] = None()

  def collect[B](pf: PartialFunction[A, B]): Option[B] = None()

  def exist(p: A => Boolean): Boolean = false

  def forall(p: A => Boolean): Boolean = true

  def contains[B >: A](elem: B): Boolean = false

  def doOnDefined(f: A => Unit): Option[A] = this

  def doOnEmpty(f: () => Unit): Option[A] = {
    f()
    this
  }
}
