package experiments

import experiments.monadics.instances.Maybe

import scala.language.higherKinds

package object monadics {
  implicit def addMonoid[T, M[_]](monoid: M[T])(implicit ev: Monoid[M]) = {
    new {
      def mappend[S >: T]: M[S] => M[S] = ev.mappend(monoid, _)
    }
  }

  implicit def addFunctor[A, F[_]](functor: F[A])(implicit ev: Functor[F]) = {
    new {
      def map[B](f: A => B): F[B] = ev.map(f, functor)
    }
  }

  implicit def addApplicativeFunc[A, B, App[_]](applicative: App[A => B])(implicit ev: Applicative[App]) = {
    new {
      def <*> : App[A] => App[B] = ev.<*>(applicative, _)
    }
  }

  implicit def addApplicative[A, App[_]](applicative: App[A])(implicit ev: Applicative[App]) = {
    new {
      def *>[B]: App[B] => App[B] = ev.*>(applicative, _)

      def <*[B]: App[B] => App[A] = ev.<*(applicative, _)

      def <**>[B]: App[A => B] => App[B] = ev.<**>(applicative, _)
    }
  }

  implicit def addAlternative[A, Alt[_]](alternative: Alt[A])(implicit ev: Alternative[Alt]) = {
    new {
      def getOrElse[B >: A](default: => B): B = ev.getOrElse(alternative, default)

      def orElse : Alt[A] => Alt[A] = ev.orElse(alternative, _)

      def some: Alt[List[A]] = ev.some(alternative)

      def many: Alt[List[A]] = ev.many(alternative)

      def maybe: Alt[Maybe[A]] = ev.maybe(alternative)
    }
  }

  implicit def addMonad[A, M[_]](monad: M[A])(implicit ev: Monad[M]) = {
    new {
      def >>=[B](f: A => M[B]): M[B] = ev.flatMap(monad, f)

      def flatMap[B](f: A => M[B]): M[B] = >>=(f)

      def >>[B]: M[B] => M[B] = ev.andThen(monad, _)

      def andThen[B]: M[B] => M[B] = >>

      def <<[B]: M[B] => M[A] = ev.thenAnd(monad, _)

      def thenAnd[B]: M[B] => M[A] = <<

      def flatten[B](implicit ev2: A <:< M[B]): M[B] = ev.flatten(monad)
    }
  }

  implicit def addMonadPlus[A, MP[_]](monadPlus: MP[A])(implicit ev: MonadPlus[MP]) = {
    new {
      def mplus: MP[A] => MP[A] = ev.mplus(monadPlus, _)

      def filter(predicate: A => Boolean): MP[A] = ev.filter(predicate, monadPlus)
    }
  }
}
