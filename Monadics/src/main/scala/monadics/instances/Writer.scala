package monadics.instances

import monadics.structures.{Applicative, Monad, Monoid, Traverse}

class Writer[W, A](val run: (A, W))(implicit wIsMonoid: Monoid[W], monad: Monad[Writer[W, ?]] with Traverse[Writer[W, ?]]) {

  def value: A = run._1

  def log: W = run._2

  def map[B](f: A => B): Writer[W, B] = monad.map(this)(f)

  def <*>[B, C](other: Writer[W, B])(implicit ev: A <:< (B => C)): Writer[W, C] = monad.<*>(this.map(ev), other)

  def flatMap[B](f: A => Writer[W, B]): Writer[W, B] = monad.flatMap(this)(f)

  def foldLeft[B](z: => B)(f: (=> B, A) => B): B = monad.foldLeft(this, z)(f)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = monad.foldRight(this, z)(f)

  def traverse[G[_], B](f: A => G[B])(implicit applicative: Applicative[G]): G[Writer[W, B]] = {
    monad.traverse(this)(f)
  }

  def sequence[G[_], B](implicit ev: A <:< G[B], applicative: Applicative[G]): G[Writer[W, B]] = {
    monad.sequence[G, B](this.map(ev))
  }
}

object Writer {

  def apply[W, A](a: A, w: W)(implicit wIsMonoid: Monoid[W], monad: Monad[Writer[W, ?]] with Traverse[Writer[W, ?]]): Writer[W, A] = {
    new Writer((a, w))
  }

  def tell[W, A](w: W)(implicit wIsMonoid: Monoid[W], monad: Monad[Writer[W, ?]] with Traverse[Writer[W, ?]]): Writer[W, Unit] = {
    new Writer((), w)
  }

  implicit def writerIsMonad[W](implicit wIsMonoid: Monoid[W]): Monad[Writer[W, ?]] with Traverse[Writer[W, ?]] = new Monad[Writer[W, ?]] with Traverse[Writer[W, ?]] {
    implicit val self: Monad[Writer[W, ?]] with Traverse[Writer[W, ?]] = this

    def create[A](a: A): Writer[W, A] = Writer(a, wIsMonoid.empty)

    override def map[A, B](writer: Writer[W, A])(f: A => B): Writer[W, B] = {
      val (x, v) = writer.run

      Writer(f(x), v)
    }

    override def <*>[A, B](appFunc: Writer[W, A => B], appA: Writer[W, A]): Writer[W, B] = {
      val (f, v) = appFunc.run
      val (x, w) = appA.run

      Writer(f(x), wIsMonoid.combine(v, w))
    }

    def flatMap[A, B](writer: Writer[W, A])(f: A => Writer[W, B]): Writer[W, B] = {
      val (x, v) = writer.run
      val (y, w) = f(x).run

      Writer(y, wIsMonoid.combine(v, w))
    }

    override def foldMap[A, B](writer: Writer[W, A])(f: A => B)(implicit mb: Monoid[B]): B = {
      val (a, _) = writer.run
      f(a)
    }

    override def foldLeft[A, B](writer: Writer[W, A], z: => B)(f: (=> B, A) => B): B = {
      val (a, _) = writer.run
      f(z, a)
    }

    override def foldRight[A, B](writer: Writer[W, A], z: => B)(f: (A, => B) => B): B = {
      val (a, _) = writer.run
      f(a, z)
    }

    def traverse[G[_], A, B](writer: Writer[W, A])(f: A => G[B])(implicit applicative: Applicative[G]): G[Writer[W, B]] = {
      val (a, w) = writer.run
      applicative.map(f(a))(b => Writer(b, w))
    }
  }
}
