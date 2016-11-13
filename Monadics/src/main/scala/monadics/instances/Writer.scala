package monadics.instances

import monadics.structures.{Monad, Monoid}

class Writer[W, A](val run: (A, W))(implicit wIsMonoid: Monoid[W], monad: Monad[Writer[W, ?]]) {

  def value: A = run._1

  def log: W = run._2

  def map[B](f: A => B): Writer[W, B] = monad.map(this)(f)

  def <*>[B, C](other: Writer[W, B])(implicit ev: A <:< (B => C)): Writer[W, C] = monad.<*>(this.map(ev), other)

  def flatMap[B](f: A => Writer[W, B]): Writer[W, B] = monad.flatMap(this)(f)
}

object Writer {

  def apply[W, A](a: A, w: W)(implicit wIsMonoid: Monoid[W], monad: Monad[Writer[W, ?]]): Writer[W, A] = {
    new Writer((a, w))
  }

  def tell[W, A](w: W)(implicit wIsMonoid: Monoid[W], monad: Monad[Writer[W, ?]]): Writer[W, Unit] = {
    new Writer((), w)
  }

  implicit def writerIsMonad[W](implicit wIsMonoid: Monoid[W]): Monad[Writer[W, ?]] = new Monad[Writer[W, ?]] {
    def create[A](a: A): Writer[W, A] = Writer(a, wIsMonoid.empty)

    override def map[A, B](writer: Writer[W, A])(f: A => B): Writer[W, B] = {
      val (x, v) = writer.run

      Writer(f(x), v)
    }

    override def <*>[A, B](appFunc: Writer[W, A => B], appA: Writer[W, A]): Writer[W, B] = {
      val (f, v) = appFunc.run
      val (x, w) = appA.run

      Writer(f(x), wIsMonoid.append(v, w))
    }

    def flatMap[A, B](writer: Writer[W, A])(f: A => Writer[W, B]): Writer[W, B] = {
      val (x, v) = writer.run
      val (y, w) = f(x).run

      Writer(y, wIsMonoid.append(v, w))
    }
  }
}
