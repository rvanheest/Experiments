package experiments.tryMonad

import scala.util.control.NonFatal

sealed trait Try[+T] {

  def getOrElse[U >: T](default: => U): U

  def getOrCatch[U >: T](handle: Throwable => U): U

  def orElse[U >: T](default: => Try[U]): Try[U]

  def recoverWith[U >: T](f: PartialFunction[Throwable, Try[U]]): Try[U]

  def recover[U >: T](f: PartialFunction[Throwable, U]): Try[U]

  def foreach[U](f: T => U): Unit

  def flatMap[U](f: T => Try[U]): Try[U]

  def flatten[U](implicit ev: T <:< Try[U]): Try[U]

  def map[U](f: T => U): Try[U]

  def filter(predicate: T => Boolean): Try[T]

  def doOnSuccess(f: T => Unit): Try[T]

  def doOnError(f: Throwable => Unit): Try[T]
}

object Try {
  def apply[T](r: => T): Try[T] = {
    try {
      Success(r)
    }
    catch {
      case e if NonFatal(e) => Failure(e)
    }
  }

  def error[T](exception: Throwable): Try[T] = Failure(exception)
}

private final case class Success[+T](value: T) extends Try[T] {

  def getOrElse[U >: T](default: => U): U = value

  def getOrCatch[U >: T](handle: Throwable => U): U = value

  def orElse[U >: T](default: => Try[U]): Try[U] = this

  def recoverWith[U >: T](f: PartialFunction[Throwable, Try[U]]): Try[U] = this

  def recover[U >: T](f: PartialFunction[Throwable, U]): Try[U] = this

  def foreach[U](f: T => U): Unit = f(value)

  def flatMap[U](f: T => Try[U]): Try[U] = f(value)

  def flatten[U](implicit ev: T <:< Try[U]): Try[U] = value

  def map[U](f: T => U): Try[U] = f.andThen(Success(_))(value)

  def filter(predicate: T => Boolean): Try[T] = {
    if (predicate(value))
      this
    else
      Failure(new NoSuchElementException(s"Predicate does not hold for $value"))
  }

  def doOnSuccess(f: T => Unit): Try[T] = {
    try {
      f(value)
      this
    }
    catch {
      case e if NonFatal(e) => Failure(e)
    }
  }

  def doOnError(f: Throwable => Unit): Try[T] = this
}

private final case class Failure[+T](exception: Throwable) extends Try[T] {

  def getOrElse[U >: T](default: => U): U = default

  def getOrCatch[U >: T](handle: Throwable => U): U = handle(exception)

  def orElse[U >: T](default: => Try[U]): Try[U] = {
    try {
      default
    }
    catch {
      case e if NonFatal(e) => Failure(e)
    }
  }

  def recoverWith[U >: T](f: PartialFunction[Throwable, Try[U]]): Try[U] = {
    try {
      if (f isDefinedAt exception)
        f(exception)
      else this
    }
    catch {
      case e if NonFatal(e) => Failure(e)
    }
  }

  def recover[U >: T](f: PartialFunction[Throwable, U]): Try[U] = {
    try {
      if (f isDefinedAt exception)
        f.andThen(Success(_))(exception)
      else this
    }
    catch {
      case e if NonFatal(e) => Failure(e)
    }
  }

  def foreach[U](f: T => U): Unit = ()

  def flatMap[U](f: T => Try[U]): Try[U] = this.asInstanceOf[Try[U]]

  def flatten[U](implicit ev: T <:< Try[U]): Try[U] = this.asInstanceOf[Try[U]]

  def map[U](f: T => U): Try[U] = this.asInstanceOf[Try[U]]

  def filter(predicate: T => Boolean): Try[T] = this

  def doOnSuccess(f: T => Unit): Try[T] = this

  def doOnError(f: Throwable => Unit): Try[T] = {
    try {
      f(exception)
      this
    }
    catch {
      case e if NonFatal(e) => Failure(e)
    }
  }
}
