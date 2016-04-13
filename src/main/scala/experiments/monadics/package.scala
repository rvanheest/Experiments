package experiments

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

      def liftA[B](f: A => B): App[B] = ev.liftA(f, applicative)

      def liftA2[B, C]: App[B] => ((A, B) => C) => App[C] = {
        appB => f => ev.liftA2(f.curried, applicative, appB)
      }

      def liftA3[B, C, D]: App[B] => App[C] => ((A, B, C) => D) => App[D] = {
        appB => appC => f => ev.liftA3(f.curried, applicative, appB, appC)
      }
    }
  }

  implicit def addAlternative[A, Alt[_]](alternative: Alt[A])(implicit ev: Alternative[Alt]) = {
    new {
      def <|> : Alt[A] => Alt[A] = ev.<|>(alternative, _)

      def some: Alt[List[A]] = ev.some(alternative)

      def many: Alt[List[A]] = ev.many(alternative)
    }
  }

  implicit def addMonad[A, M[_]](monad: M[A])(implicit ev: Monad[M]) = {
    new {
      def >>=[B](f: A => M[B]): M[B] = ev.>>=(monad, f)

      def flatMap[B](f: A => M[B]): M[B] = >>=(f)

      def >>[B]: M[B] => M[B] = ev.>>(monad, _)

      def andThen[B]: M[B] => M[B] = >>

      def <<[B]: M[B] => M[A] = ev.<<(monad, _)

      def thenAnd[B]: M[B] => M[A] = <<

      def liftM[B](f: A => B): M[B] = ev.liftM(f, monad)

      def liftM2[B, C]: M[B] => ((A, B) => C) => M[C] = {
        mB => f => ev.liftM2(f.curried, monad, mB)
      }

      def liftM3[B, C, D]: (M[B], M[C]) => ((A, B, C) => D) => M[D] = {
        (mB, mC) => f => ev.liftM3(f.curried, monad, mB, mC)
      }
    }
  }

  implicit def addMonadPlus[A, MP[_]](monadPlus: MP[A])(implicit ev: MonadPlus[MP]) = {
    new {
      def mplus: MP[A] => MP[A] = ev.mplus(monadPlus, _)
    }
  }
}
