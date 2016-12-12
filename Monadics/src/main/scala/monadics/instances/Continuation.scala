package monadics.instances

import monadics.structures.{Equals, Monad}

class Continuation[R, A](c: (A => R) => R)(implicit monad: Monad[Continuation[R, ?]]) {

  def run(f: A => R): R = c(f)

  def map[B](f: A => B): Continuation[R, B] = monad.map(this)(f)

  def <*>[B, C](other: Continuation[R, B])(implicit ev: Continuation[R, A] <:< Continuation[R, (B => C)]): Continuation[R, C] = monad.<*>(this, other)

  def flatMap[B](f: A => Continuation[R, B]): Continuation[R, B] = monad.flatMap(this)(f)
}

object Continuation {

  def callCC[R, A, B](f: (A => Continuation[R, B]) => Continuation[R, A])(implicit monad: Monad[Continuation[R, ?]]): Continuation[R, A] = {
    new Continuation[R, A](aToR => f(a => new Continuation[R, B](_ => aToR(a))).run(aToR))
  }

  def apply[R, A](c: (A => R) => R)(implicit monad: Monad[Continuation[R, ?]]): Continuation[R, A] = {
    new Continuation[R, A](c)
  }

  def from[R, A](a: A)(implicit monad: Monad[Continuation[R, ?]]): Continuation[R, A] = monad.create(a)

  implicit def continuationIsEquals[R, A](implicit fEquals: Equals[(A => R) => R]): Equals[Continuation[R, A]] = {
    Equals.create((r1, r2) => fEquals.equals(r1.run, r2.run))
  }

  implicit def continuationIsMonad[R]: Monad[Continuation[R, ?]] = new Monad[Continuation[R, ?]] {
    def create[A](a: A): Continuation[R, A] = new Continuation(_(a))

    override def map[A, B](functor: Continuation[R, A])(f: A => B): Continuation[R, B] = {
      new Continuation(bToR => functor.run(f andThen bToR))
    }

    override def <*>[A, B](appFunc: Continuation[R, (A) => B], appA: Continuation[R, A]): Continuation[R, B] = {
      new Continuation(bToR => appFunc.run(aToB => appA.run(aToB andThen bToR)))
    }

    def flatMap[A, B](monad: Continuation[R, A])(f: A => Continuation[R, B]): Continuation[R, B] = {
      new Continuation(bToR => monad.run(f(_).run(bToR)))
    }
  }
}
