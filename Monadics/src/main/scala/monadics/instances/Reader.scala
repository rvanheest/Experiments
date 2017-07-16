package monadics.instances

import monadics.structures.{ Comonad, Equals, Monad, Monoid }

import scala.language.implicitConversions

class Reader[R, A](f: R => A)(implicit monad: Monad[Reader[R, ?]]) {

  def run(r: R): A = f(r)

  def map[B](f: A => B): Reader[R, B] = monad.map(this)(f)

  def as[B](b: => B): Reader[R, B] = monad.as(this, b)

  def void: Reader[R, Unit] = monad.void(this)

  def zipWith[B](f: A => B): Reader[R, (A, B)] = monad.zipWith(this)(f)

  def <*>[B, C](other: Reader[R, B])(implicit ev: Reader[R, A] <:< Reader[R, (B => C)]): Reader[R, C] = monad.<*>(this, other)

  def *>[B](other: Reader[R, B]): Reader[R, B] = monad.*>(this, other)

  def <*[B](other: Reader[R, B]): Reader[R, A] = monad.<*(this, other)

  def <**>[B](other: Reader[R, A => B]): Reader[R, B] = monad.<**>(this, other)

  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = monad.flatMap(this)(f)

  def andThen[B](other: Reader[R, B]): Reader[R, B] = monad.andThen(this, other)

  def thenAnd[B](other: Reader[R, B]): Reader[R, A] = monad.thenAnd(this, other)

  def flatten[B](implicit ev: A <:< Reader[R, B]): Reader[R, B] = monad.flatten(this)
}

object Reader {

  def apply[R, A](f: R => A): Reader[R, A] = new Reader(f)

  implicit def readerIsFunction[R, A](reader: Reader[R, A]): R => A = reader.run

  implicit def readerIsEquals[R, A](implicit fEquals: Equals[R => A]): Equals[Reader[R, A]] = {
    Equals.create((r1, r2) => fEquals.equals(r1.run, r2.run))
  }

  implicit def readerIsMonad[R]: Monad[Reader[R, ?]] = new Monad[Reader[R, ?]] {
    def create[A](a: A): Reader[R, A] = new Reader(_ => a)

    override def map[A, B](functor: Reader[R, A])(f: A => B): Reader[R, B] = {
      Reader(f compose functor.run)
    }

    override def <*>[A, B](appFunc: Reader[R, A => B], appA: Reader[R, A]): Reader[R, B] = {
      Reader(r => appFunc.run(r)(appA.run(r)))
    }

    def flatMap[A, B](monad: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = {
      Reader(r => f(monad.run(r)).run(r))
    }
  }

  implicit def readerIsComonad[R](implicit ev: Monoid[R]): Comonad[Reader[R, ?]] = new Comonad[Reader[R, ?]] {
    def map[A, B](reader: Reader[R, A])(f: A => B): Reader[R, B] = {
      Reader(f compose reader.run)
    }

    def extract[A](reader: Reader[R, A]): A = reader.run(ev.empty)

    def extend[A, B](reader: Reader[R, A])(f: Reader[R, A] => B): Reader[R, B] = {
      Reader(r => f(Reader((r2: R) => reader.run(ev.combine(r2, r)))))
    }
  }
}
