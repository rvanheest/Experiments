package monadics.instances

import monadics.structures.Monad

class Reader[R, A](f: R => A)(implicit monad: Monad[Reader[R, ?]]) {

  def run(r: R): A = f(r)

  def map[B](f: A => B): Reader[R, B] = monad.map(this)(f)

  def <*>[B, C](other: Reader[R, B])(implicit ev: A <:< (B => C)): Reader[R, C] = monad.<*>(this.map(ev), other)

  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = monad.flatMap(this)(f)
}

object Reader {

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
}
