package monadics.instances

import monadics.structures._

case class NonEmptyList[A](head: A, tail: List[A])(implicit semigroup: Semigroup[NonEmptyList[A]], monadTraverse: Monad[NonEmptyList] with Traverse[NonEmptyList]) {

  def map[B](f: A => B): NonEmptyList[B] = monadTraverse.map(this)(f)

  def as[B](b: => B): NonEmptyList[B] = monadTraverse.as(this, b)

  def void: NonEmptyList[Unit] = monadTraverse.void(this)

  def zipWith[B](f: A => B): NonEmptyList[(A, B)] = monadTraverse.zipWith(this)(f)

  def <*>[B, C](other: NonEmptyList[B])(implicit ev: A <:< (B => C)): NonEmptyList[C] = monadTraverse.<*>(map(ev), other)

  def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] = monadTraverse.flatMap(this)(f)

  def ++(other: NonEmptyList[A]): NonEmptyList[A] = semigroup.combine(this, other)

  def foldLeft[B](z: => B)(f: (=> B, A) => B): B = monadTraverse.foldLeft(this, z)(f)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = monadTraverse.foldRight(this, z)(f)

  def foldMap[B](f: A => B)(implicit mb: Monoid[B]): B = monadTraverse.foldMap(this)(f)

  def traverse[G[_], B](f: A => G[B])(implicit applicative: Applicative[G]): G[NonEmptyList[B]] = {
    monadTraverse.traverse(this)(f)
  }

  def sequence[G[_], B](implicit ev: A <:< G[B], applicative: Applicative[G]): G[NonEmptyList[B]] = {
    monadTraverse.sequence[G, B](map(ev))
  }
}

object NonEmptyList {

  def apply[A](head: A, tail: A*): NonEmptyList[A] = {
    new NonEmptyList(head, tail.toList)
  }

  implicit def NELisSemigroup[A]: Semigroup[NonEmptyList[A]] = Semigroup.create {
    (a1, a2) => NonEmptyList(a1.head, a1.tail ++ (a2.head :: a2.tail))
  }

  implicit val NELisMonad: Monad[NonEmptyList] with Traverse[NonEmptyList] = new Monad[NonEmptyList] with Traverse[NonEmptyList] {

    def create[A](a: A): NonEmptyList[A] = NonEmptyList(a, Nil)

    def flatMap[A, B](monad: NonEmptyList[A])(f: A => NonEmptyList[B]): NonEmptyList[B] = {
      val NonEmptyList(head, tail) = f(monad.head)

      def foldFunc(nelb: NonEmptyList[B], xs: List[B]): List[B] = {
        (nelb.head :: nelb.tail) ++ xs
      }

      NonEmptyList(head, tail ++ monad.tail.map(f).foldRight(List.empty[B])(foldFunc))
    }

    override def foldLeft[A, B](nel: NonEmptyList[A], z: => B)(f: (=> B, A) => B): B = {
      val NonEmptyList(x, xs) = nel
      xs.foldLeft(f(z, x))(f(_, _))
    }

    override def foldRight[A, B](nel: NonEmptyList[A], z: => B)(f: (A, => B) => B): B = {
      val NonEmptyList(x, xs) = nel
      f(x, xs.foldRight(z)(f(_, _)))
    }

    def traverse[G[_], A, B](nel: NonEmptyList[A])(f: A => G[B])(implicit applicative: Applicative[G]): G[NonEmptyList[B]] = {
      nel match {
        case NonEmptyList(x, Nil) => applicative.map(f(x))(b => NonEmptyList(b))
        case NonEmptyList(x, xs) =>
          val gB: G[B] = f(x)
          val gListB: G[List[B]] = xs.foldRight(applicative.create(List.empty[B]))((a: A, acc: G[List[B]]) => {
            applicative.<**>(acc, applicative.map(f(a))((b: B) => (bs: List[B]) => b :: bs))
          })

          applicative.<**>(gB, applicative.map(gListB)((bs: List[B]) => (b: B) => NonEmptyList(b, bs)))
      }
    }
  }
}
