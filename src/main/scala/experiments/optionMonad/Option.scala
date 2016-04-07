package experiments.optionMonad

import scala.language.implicitConversions

sealed trait Option[+A] {

  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](default: => Option[B]): Option[B]

  def ifPresent[U](f: A => U): Unit

  def flatMap[B](f: A => Option[B]): Option[B]

  def flatten[B](implicit ev: A <:< Option[B]): Option[B]

  def map[B](f: A => B): Option[B]

  def filter(p: A => Boolean): Option[A]

  def filterNot(p: A => Boolean): Option[A] = filter(!p(_))

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

  implicit def scala2MyOption[A](xo: scala.Option[A]): Option[A] = {
    xo.map(Option(_)).getOrElse(Option.empty)
  }

  def apply[A](x: A): Option[A] = {
    if (x == null)
      None
    else
      Some(x)
  }

  def empty: Option[Nothing] = None
}

private final case class Some[+A](value: A) extends Option[A] {

  def getOrElse[B >: A](default: => B): B = value

  def orElse[B >: A](default: => Option[B]): Option[B] = this

  def ifPresent[U](f: A => U): Unit = f(value)

  def flatMap[B](f: A => Option[B]): Option[B] = f(value)

  def flatten[B](implicit ev: A <:< Option[B]): Option[B] = ev(value)

  def map[B](f: A => B): Option[B] = Some(f(value))

  def filter(p: A => Boolean): Option[A] = {
    if (p(value))
      this
    else
      None
  }

  def collect[B](pf: PartialFunction[A, B]): Option[B] = pf.lift(value)

  def exist(p: A => Boolean): Boolean = p(value)

  def forall(p: A => Boolean): Boolean = p(value)

  def contains[B >: A](elem: B): Boolean = value == elem

  def doOnDefined(f: A => Unit): Option[A] = {
    f(value)
    this
  }

  def doOnEmpty(f: () => Unit): Option[A] = this
}

private case object None extends Option[Nothing] {

  def getOrElse[B >: Nothing](default: => B): B = default

  def orElse[B >: Nothing](default: => Option[B]): Option[B] = default

  def ifPresent[U](f: Nothing => U): Unit = ()

  def flatMap[B](f: Nothing => Option[B]): Option[B] = None

  def flatten[B](implicit ev: Nothing <:< Option[B]): Option[B] = None

  def map[B](f: Nothing => B): Option[B] = None

  def filter(p: Nothing => Boolean): Option[Nothing] = None

  override def filterNot(p: Nothing => Boolean): Option[Nothing] = None

  def collect[B](pf: PartialFunction[Nothing, B]): Option[B] = None

  def exist(p: Nothing => Boolean): Boolean = false

  def forall(p: Nothing => Boolean): Boolean = true

  def contains[B >: Nothing](elem: B): Boolean = false

  def doOnDefined(f: Nothing => Unit): Option[Nothing] = this

  def doOnEmpty(f: () => Unit): Option[Nothing] = {
    f()
    this
  }
}
