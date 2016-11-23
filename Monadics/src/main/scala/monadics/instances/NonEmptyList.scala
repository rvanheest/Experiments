package monadics.instances

import monadics.structures.{Monad, Semigroup}

case class NonEmptyList[A](head: A, tail: List[A])(implicit semigroup: Semigroup[NonEmptyList[A]], monad: Monad[NonEmptyList]) {

  def map[B](f: A => B): NonEmptyList[B] = monad.map(this)(f)

  def as[B](b: => B): NonEmptyList[B] = monad.as(this, b)

  def void: NonEmptyList[Unit] = monad.void(this)

  def zipWith[B](f: A => B): NonEmptyList[(A, B)] = monad.zipWith(this)(f)

  def <*>[B, C](other: NonEmptyList[B])(implicit ev: A <:< (B => C)): NonEmptyList[C] = monad.<*>(map(ev), other)

  def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] = monad.flatMap(this)(f)

  def ++(other: NonEmptyList[A]): NonEmptyList[A] = semigroup.combine(this, other)
}

object NonEmptyList {

  def apply[A](head: A, tail: A*): NonEmptyList[A] = {
    new NonEmptyList(head, tail.toList)
  }

  implicit def NELisSemigroup[A]: Semigroup[NonEmptyList[A]] = Semigroup.create {
    (a1, a2) => NonEmptyList(a1.head, a1.tail ++ (a2.head :: a2.tail))
  }

  implicit def NELisMonad: Monad[NonEmptyList] = new Monad[NonEmptyList] {

    def create[A](a: A): NonEmptyList[A] = NonEmptyList(a, Nil)

    def flatMap[A, B](monad: NonEmptyList[A])(f: A => NonEmptyList[B]): NonEmptyList[B] = {
      val NonEmptyList(head, tail) = f(monad.head)

      def foldFunc(nelb: NonEmptyList[B], xs: List[B]): List[B] = {
        (nelb.head :: nelb.tail) ++ xs
      }

      NonEmptyList(head, tail ++ monad.tail.map(f).foldRight(List.empty[B])(foldFunc))
    }
  }
}
