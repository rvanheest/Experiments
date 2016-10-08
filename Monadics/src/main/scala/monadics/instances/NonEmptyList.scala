package monadics.instances

import monadics.structures.{Monad, Semigroup}

case class NonEmptyList[A](head: A, tail: List[A])(implicit semigroup: Semigroup[NonEmptyList[A]], monad: Monad[NonEmptyList]) {

  def map[B](f: A => B): NonEmptyList[B] = monad.map(this)(f)

  def flatMap[B](f: A => NonEmptyList[B]): NonEmptyList[B] = monad.flatMap(this)(f)

  def ++(other: NonEmptyList[A]): NonEmptyList[A] = semigroup.append(this, other)
}

object NonEmptyList {

  def apply[A](head: A, tail: A*): NonEmptyList[A] = {
    new NonEmptyList(head, tail.toList)
  }

  implicit def NELisSemigroup[A]: Semigroup[NonEmptyList[A]] = new Semigroup[NonEmptyList[A]] {
    def append(a1: NonEmptyList[A], a2: => NonEmptyList[A]): NonEmptyList[A] = {
      NonEmptyList(a1.head, a1.tail ++ (a2.head :: a2.tail))
    }
  }

  implicit def NELisMonad: Monad[NonEmptyList] = new Monad[NonEmptyList] {

    def fail[A](e: Throwable): NonEmptyList[A] = throw e

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
