package monadics.instances

import monadics.structures._

import scala.language.higherKinds

trait either {

  implicit def eitherIsEquals[L, R](implicit lEquals: Equals[L], rEquals: Equals[R]): Equals[Either[L, R]] = {
    Equals.create {
      case (Left(l1), Left(l2)) => lEquals.equals(l1, l2)
      case (Right(r1), Right(r2)) => rEquals.equals(r1, r2)
      case _ => false
    }
  }

  implicit def eitherIsSemigroup[L, R]: Semigroup[Either[L, R]] = Semigroup.create[Either[L, R]] {
    case (Left(_), a2) => a2
    case (a, _) => a
  }

  implicit def eitherIsMonad[L] = new Monad[Either[L, ?]] with Traverse[Either[L, ?]] {
    def create[R](r: R): Either[L, R] = Right(r)

    override def map[R, R2](functor: Either[L, R])(f: R => R2): Either[L, R2] = {
      functor.right.map(f)
    }

    override def <*>[R, R2](appFunc: Either[L, R => R2], appA: Either[L, R]): Either[L, R2] = {
      appFunc.right.flatMap(appA.right.map)
    }

    def flatMap[R, R2](monad: Either[L, R])(f: R => Either[L, R2]): Either[L, R2] = {
      monad.right.flatMap(f)
    }

    override def foldMap[A, B](fa: Either[L, A])(f: (A) => B)(implicit mb: Monoid[B]): B = {
      fa.fold(_ => mb.empty, f)
    }

    override def foldLeft[A, B](fa: Either[L, A], z: => B)(f: (=> B, A) => B): B = {
      fa.fold(_ => z, f(z, _))
    }

    override def foldRight[A, B](fa: Either[L, A], z: => B)(f: (A, => B) => B): B = {
      fa.fold(_ => z, f(_, z))
    }

    override def size[A](fa: Either[L, A]): Int = fa.fold(_ => 0, _ => 1)

    override def isEmpty[A](fa: Either[L, A]): Boolean = fa.isLeft

    def traverse[G[_], A, B](fa: Either[L, A])(f: A => G[B])(implicit applicative: Applicative[G]): G[Either[L, B]] = {
      fa.fold((x: L) => applicative.create(Left(x)), (y: A) => applicative.map(f(y))(Right(_)))
    }
  }

  implicit class EitherSemigroup[L, R](val either: Either[L, R])(implicit semigroup: Semigroup[Either[L, R]]) {
    def orElse(other: => Either[L, R]): Either[L, R] = semigroup.combine(either, other)
  }

  implicit class EitherMonadOperators[L, R](val either: Either[L, R])(implicit monad: Monad[Either[L, ?]] with Traverse[Either[L, ?]]) {
    def as[R2](r2: => R2): Either[L, R2] = monad.as(either, r2)

    def void: Either[L, Unit] = monad.void(either)

    def zipWith[R2](f: R => R2): Either[L, (R, R2)] = monad.zipWith(either)(f)

    def traverse[G[_], R2](f: R => G[R2])(implicit applicative: Applicative[G]): G[Either[L, R2]] = {
      monad.traverse(either)(f)
    }

    def sequence[G[_], R2](implicit ev: Either[L, R] <:< Either[L, G[R2]], applicative: Applicative[G]): G[Either[L, R2]] = {
      monad.sequence(either)
    }
  }
}

trait eitherLeft {

  type LeftEither[+L, +R] = Either.LeftProjection[L, R]

  implicit def leftEitherIsEquals[L, R](implicit lEquals: Equals[L], rEquals: Equals[R]): Equals[LeftEither[L, R]] = {
    Equals.create({
          case (Either.LeftProjection(Left(l1)), Either.LeftProjection(Left(l2))) => lEquals.equals(l1, l2)
          case (Either.LeftProjection(Right(r1)), Either.LeftProjection(Right(r2))) => rEquals.equals(r1, r2)
          case _ => false
        })
  }

  implicit def eitherLeftIsSemigroup[L, R]: Semigroup[LeftEither[L, R]] = Semigroup.create[LeftEither[L, R]] {
    case (Either.LeftProjection(Right(_)), a) => a
    case (a, _) => a
  }

  implicit def eitherLeftIsMonad[R] = new Monad[LeftEither[?, R]] with Traverse[LeftEither[?, R]] {
    override def create[L](l: L): LeftEither[L, R] = Left(l).left

    override def map[L, L2](functor: LeftEither[L, R])(f: L => L2): LeftEither[L2, R] = {
      functor.map(f).left
    }

    override def <*>[L, L2](appFunc: LeftEither[L => L2, R], appA: LeftEither[L, R]): LeftEither[L2, R] = {
      appFunc.flatMap(appA.map).left
    }

    override def flatMap[L, L2](monad: LeftEither[L, R])(f: L => LeftEither[L2, R]): LeftEither[L2, R] = {
      monad.flatMap(f andThen (_.e)).left
    }

    override def foldMap[A, B](fa: LeftEither[A, R])(f: A => B)(implicit mb: Monoid[B]): B = {
      fa.e.fold(f, _ => mb.empty)
    }

    override def foldLeft[A, B](fa: LeftEither[A, R], z: => B)(f: (=> B, A) => B): B = {
      fa.e.fold(f(z, _), _ => z)
    }

    override def foldRight[A, B](fa: LeftEither[A, R], z: => B)(f: (A, => B) => B): B = {
      fa.e.fold(f(_, z), _ => z)
    }

    override def size[A](fa: LeftEither[A, R]): Int = fa.e.fold(_ => 1, _ => 0)

    override def isEmpty[A](fa: LeftEither[A, R]): Boolean = fa.e.isRight

    def traverse[G[_], A, B](fa: LeftEither[A, R])(f: A => G[B])(implicit applicative: Applicative[G]): G[LeftEither[B, R]] = {
      fa.e.fold(x => applicative.map(f(x))(Left(_).left), y => applicative.create(Right(y).left))
    }
  }

  implicit class LeftEitherSemigroup[L, R](val either: LeftEither[L, R])(implicit semigroup: Semigroup[LeftEither[L, R]]) {
    def orElse(other: => LeftEither[L, R]): LeftEither[L, R] = semigroup.combine(either, other)
  }

  implicit class EitherLeftMonad[L, R](val either: LeftEither[L, R])(implicit monad: Monad[LeftEither[?, R]] with Traverse[LeftEither[?, R]]) {
    def as[L2](l2: => L2): LeftEither[L2, R] = monad.as(either, l2)

    def void: LeftEither[Unit, R] = monad.void(either)

    def zipWith[L2](f: L => L2): LeftEither[(L, L2), R] = monad.zipWith(either)(f)

    def traverse[G[_], L2](f: L => G[L2])(implicit applicative: Applicative[G]): G[LeftEither[L2, R]] = {
      monad.traverse(either)(f)
    }

    def sequence[G[_], L2](implicit ev: LeftEither[L, R] <:< LeftEither[G[L2], R], applicative: Applicative[G]): G[LeftEither[L2, R]] = {
      monad.sequence(either)
    }
  }
}

trait eitherRight {

  type RightEither[+L, +R] = Either.RightProjection[L, R]

  implicit def rightEitherIsEquals[L, R](implicit lEquals: Equals[L], rEquals: Equals[R]): Equals[RightEither[L, R]] = {
    Equals.create({
          case (Either.RightProjection(Left(l1)), Either.RightProjection(Left(l2))) => lEquals.equals(l1, l2)
          case (Either.RightProjection(Right(r1)), Either.RightProjection(Right(r2))) => rEquals.equals(r1, r2)
          case _ => false
        })
  }

  implicit def eitherRightIsSemigroup[L, R]: Semigroup[RightEither[L, R]] = Semigroup.create[RightEither[L, R]] {
    case (Either.RightProjection(Left(_)), a2) => a2
    case (a, _) => a
  }

  implicit def eitherRightIsMonad[L] = new Monad[RightEither[L, ?]] with Traverse[RightEither[L, ?]] {
    override def create[R](r: R): RightEither[L, R] = Right(r).right

    override def map[R, R2](functor: RightEither[L, R])(f: R => R2): RightEither[L, R2] = {
      functor.map(f).right
    }

    override def <*>[R, R2](appFunc: RightEither[L, R => R2], appA: RightEither[L, R]): RightEither[L, R2] = {
      appFunc.flatMap(appA.map).right
    }

    override def flatMap[R, R2](monad: RightEither[L, R])(f: R => RightEither[L, R2]): RightEither[L, R2] = {
      monad.flatMap(f andThen (_.e)).right
    }

    override def foldMap[A, B](fa: RightEither[L, A])(f: A => B)(implicit mb: Monoid[B]): B = {
      fa.e.fold(_ => mb.empty, f)
    }

    override def foldLeft[A, B](fa: RightEither[L, A], z: => B)(f: (=> B, A) => B): B = {
      fa.e.fold(_ => z, f(z, _))
    }

    override def foldRight[A, B](fa: RightEither[L, A], z: => B)(f: (A, => B) => B): B = {
      fa.e.fold(_ => z, f(_, z))
    }

    override def size[A](fa: RightEither[L, A]): Int = fa.e.fold(_ => 0, _ => 1)

    override def isEmpty[A](fa: RightEither[L, A]): Boolean = fa.e.isLeft

    def traverse[G[_], A, B](fa: RightEither[L, A])(f: A => G[B])(implicit applicative: Applicative[G]): G[RightEither[L, B]] = {
      fa.e.fold(x => applicative.create(Left(x).right), y => applicative.map(f(y))(Right(_).right))
    }
  }

  implicit class LeftEitherSemigroup[L, R](val either: RightEither[L, R])(implicit semigroup: Semigroup[RightEither[L, R]]) {
    def orElse(other: => RightEither[L, R]): RightEither[L, R] = semigroup.combine(either, other)
  }

  implicit class EitherLeftMonad[L, R](val either: RightEither[L, R])(implicit monad: Monad[RightEither[L, ?]] with Traverse[RightEither[L, ?]]) {
    def as[R2](r2: => R2): RightEither[L, R2] = monad.as(either, r2)

    def void: RightEither[L, Unit] = monad.void(either)

    def zipWith[B](f: R => B): RightEither[L, (R, B)] = monad.zipWith(either)(f)

    def traverse[G[_], R2](f: R => G[R2])(implicit applicative: Applicative[G]): G[RightEither[L, R2]] = {
      monad.traverse(either)(f)
    }

    def sequence[G[_], R2](implicit ev: RightEither[L, R] <:< RightEither[L, G[R2]], applicative: Applicative[G]): G[RightEither[L, R2]] = {
      monad.sequence(either)
    }
  }
}

object either extends either with eitherLeft with eitherRight
