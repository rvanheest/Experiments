package experiments

import scala.language.higherKinds

package object monadics {
  implicit def addMonoid[T, M[_]](monoid: M[T])(implicit ev: Monoid[T, M]) = {
    new {
      def mappend[S >: T]: M[S] => M[S] = other => ev.mappend(monoid, other)
    }
  }

  implicit def addFunctor[A, F[_]](functor: F[A])(implicit ev: Functor[A, F]) = {
    new {
      def map[B](f: A => B): F[B] = ev.fmap(functor, f)
    }
  }

  implicit def addMonad[A, M[_]](monad: M[A])(implicit ev: Monad[A, M]) = {
    new {
      def flatMap[B](f: A => M[B]): M[B] = ev.flatMap(monad, f)
    }
  }

  implicit def addMonadPlus[A, MP[_]](monadPlus: MP[A])(implicit ev: MonadPlus[A, MP]) = {
    new {
      def mplus[B >: A]: MP[B] => MP[B] = other => ev.mplus(monadPlus, other)
    }
  }
}
