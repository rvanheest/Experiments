package monadics.instances

import monadics.structures.{ Comonad, Equals, Monad, Monoid }

import scala.language.implicitConversions

class Reader[R, A](f: R => A)(implicit monad: Monad[Reader[R, ?]]) {

  def run(r: R): A = f(r)

  def map[B](f: A => B): Reader[R, B] = monad.map(this)(f)

  def <*>[B, C](other: Reader[R, B])(implicit ev: Reader[R, A] <:< Reader[R, (B => C)]): Reader[R, C] = monad.<*>(this, other)

  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = monad.flatMap(this)(f)
}

object Reader {

  implicit def readerIsFunction[R, A](reader: Reader[R, A]): R => A = reader.run

  implicit def readerIsEquals[R, A](implicit fEquals: Equals[R => A]): Equals[Reader[R, A]] = {
    Equals.create((r1, r2) => fEquals.equals(r1.run, r2.run))
  }

  implicit def readerIsMonad[R]: Monad[Reader[R, ?]] = new Monad[Reader[R, ?]] {
    def create[A](a: A): Reader[R, A] = new Reader(_ => a)

    override def map[A, B](functor: Reader[R, A])(f: A => B): Reader[R, B] = {
      new Reader(f compose functor.run)
    }

    override def <*>[A, B](appFunc: Reader[R, A => B], appA: Reader[R, A]): Reader[R, B] = {
      new Reader(r => appFunc.run(r)(appA.run(r)))
    }

    def flatMap[A, B](monad: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = {
      new Reader(r => f(monad.run(r)).run(r))
    }
  }

  implicit def readerIsComonad[R](implicit ev: Monoid[R]): Comonad[Reader[R, ?]] = new Comonad[Reader[R, ?]] {
    def map[A, B](reader: Reader[R, A])(f: A => B): Reader[R, B] = {
      new Reader(f compose reader.run)
    }

    def extract[A](reader: Reader[R, A]): A = reader.run(ev.empty)

    def extend[A, B](reader: Reader[R, A])(f: Reader[R, A] => B): Reader[R, B] = {
      new Reader(r => f((r2: R) => reader.run(ev.combine(r2, r))))
    }
  }
}
