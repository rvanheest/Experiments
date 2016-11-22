package monadics.instances

import monadics.structures.{Monad, Semigroup}

trait either {

  implicit def eitherIsSemiGroup[L, R]: Semigroup[Either[L, R]] = Semigroup.create[Either[L, R]] {
    case (Left(_), a2) => a2
    case (a, _) => a
  }

  implicit class EitherSemigroup[L, R](val either: Either[L, R])(implicit semigroup: Semigroup[Either[L, R]]) {
    def orElse(other: => Either[L, R]): Either[L, R] = semigroup.combine(either, other)
  }

  implicit def eitherIsMonad[L] = new Monad[Either[L, ?]] {
    def create[R](a: R): Either[L, R] = Right(a)

    override def map[A, B](functor: Either[L, A])(f: A => B): Either[L, B] = {
      functor.right.map(f)
    }

    override def <*>[A, B](appFunc: Either[L, A => B], appA: Either[L, A]): Either[L, B] = {
      appFunc.right.flatMap(appA.right.map)
    }

    def flatMap[R, B](monad: Either[L, R])(f: R => Either[L, B]): Either[L, B] = {
      monad.right.flatMap(f)
    }
  }
}

object either extends either
