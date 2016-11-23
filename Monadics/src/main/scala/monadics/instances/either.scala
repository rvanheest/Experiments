package monadics.instances

import monadics.structures.{Monad, Semigroup}

trait either {

  implicit def eitherIsSemigroup[L, R]: Semigroup[Either[L, R]] = Semigroup.create[Either[L, R]] {
    case (Left(_), a2) => a2
    case (a, _) => a
  }

  implicit class EitherSemigroup[L, R](val either: Either[L, R])(implicit semigroup: Semigroup[Either[L, R]]) {
    def orElse(other: => Either[L, R]): Either[L, R] = semigroup.combine(either, other)
  }

  implicit def eitherIsMonad[L] = new Monad[Either[L, ?]] {
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
  }

  implicit class EitherMonadOperators[L, R](val either: Either[L, R])(implicit monad: Monad[Either[L, ?]]) {
    def as[R2](r2: => R2): Either[L, R2] = monad.as(either, r2)

    def void: Either[L, Unit] = monad.void(either)

    def zipWith[R2](f: R => R2): Either[L, (R, R2)] = monad.zipWith(either)(f)
  }
}

trait eitherLeft {

  type LeftEither[+L, +R] = Either.LeftProjection[L, R]

  implicit def eitherLeftIsSemigroup[L, R]: Semigroup[LeftEither[L, R]] = Semigroup.create[LeftEither[L, R]] {
    case (Either.LeftProjection(Right(_)), a) => a
    case (a, _) => a
  }

  implicit class LeftEitherSemigroup[L, R](val either: LeftEither[L, R])(implicit semigroup: Semigroup[LeftEither[L, R]]) {
    def orElse(other: => LeftEither[L, R]): LeftEither[L, R] = semigroup.combine(either, other)
  }

  implicit def eitherLeftIsMonad[R] = new Monad[LeftEither[?, R]] {
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
  }

  implicit class EitherLeftMonad[L, R](val either: LeftEither[L, R])(implicit monad: Monad[LeftEither[?, R]]) {
    def as[L2](l2: => L2): LeftEither[L2, R] = monad.as(either, l2)

    def void: LeftEither[Unit, R] = monad.void(either)

    def zipWith[L2](f: L => L2): LeftEither[(L, L2), R] = monad.zipWith(either)(f)
  }
}

trait eitherRight {

  type RightEither[+L, +R] = Either.RightProjection[L, R]

  implicit def eitherRightIsSemigroup[L, R]: Semigroup[RightEither[L, R]] = Semigroup.create[RightEither[L, R]] {
    case (Either.RightProjection(Left(_)), a2) => a2
    case (a, _) => a
  }

  implicit class LeftEitherSemigroup[L, R](val either: RightEither[L, R])(implicit semigroup: Semigroup[RightEither[L, R]]) {
    def orElse(other: => RightEither[L, R]): RightEither[L, R] = semigroup.combine(either, other)
  }

  implicit def eitherRightIsMonad[L] = new Monad[RightEither[L, ?]] {
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
  }

  implicit class EitherLeftMonad[L, R](val either: RightEither[L, R])(implicit monad: Monad[RightEither[L, ?]]) {
    def as[R2](r2: => R2): RightEither[L, R2] = monad.as(either, r2)

    def void: RightEither[L, Unit] = monad.void(either)

    def zipWith[B](f: R => B): RightEither[L, (R, B)] = monad.zipWith(either)(f)
  }
}

object either extends either with eitherLeft with eitherRight
