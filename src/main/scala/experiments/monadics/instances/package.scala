package experiments.monadics

import scala.language.{implicitConversions, reflectiveCalls}

package object instances {

  implicit def identityIsMonad = new Monad[Identity] {
    implicit def apply[A](a: A): Identity[A] = new Identity[A](a)

    def map[A, B](f: A => B, functor: Identity[A]): Identity[B] = {
      new Identity[B](f(functor.id))
    }

    def >>=[A, B](monad: Identity[A], f: (A) => Identity[B]): Identity[B] = {
      f(monad.id)
    }
  }

  implicit def maybeIsMonoid = new Monoid[Maybe] {
    def mappend[T, S >: T](monoid: Maybe[T], other: Maybe[S]): Maybe[S] = {
      monoid match {
        case Just(x) => Just(x)
        case None => other
      }
    }
  }

  implicit def maybeIsMonadPlus = new MonadPlus[Maybe] {
    implicit def apply[A](a: A): Maybe[A] = Maybe.apply(a)

    implicit def empty[A]: Maybe[A] = Maybe.empty

    def map[A, B](f: A => B, functor: Maybe[A]): Maybe[B] = {
      functor match {
        case Just(a) => Just(f(a))
        case None => None
      }
    }

    def <|>[A](alt1: Maybe[A], alt2: Maybe[A]): Maybe[A] = {
      alt1 match {
        case None => alt2
        case _ => alt1
      }
    }

    def >>=[A, B](monad: Maybe[A], f: A => Maybe[B]): Maybe[B] = {
      monad match {
        case Just(a) => f(a)
        case None => None
      }
    }
  }

  implicit def stateIsMonad[S] = new Monad[({ type s[x] = State[S, x] })#s] {

    implicit def apply[B](b: B): State[S, B] = new State(s => (b, s))

    def map[A, B](f: A => B, state: State[S, A]): State[S, B] = {
      new State[S, B](s => {
        val (a, s2) = state.state(s)
        (f(a), s2)
      })
    }

    override def <*>[A, B](stateAB: State[S, A => B], stateA: State[S, A]): State[S, B] = {
      new State[S, B](s => {
        val (aToB, s2) = stateAB.state(s)
        val (a, s3) = stateA.state(s2)
        (aToB(a), s3)
      })
    }

    override def *>[A, B](stateA: State[S, A], stateB: State[S, B]): State[S, B] = {
      new State[S, B](s => {
        val (_, s2) = stateA.state(s)
        stateB.state(s2)
      })
    }

    override def <*[A, B](stateA: State[S, A], stateB: State[S, B]): State[S, A] = {
      new State[S, A](s => {
        val (a, s2) = stateA.state(s)
        val (_, s3) = stateB.state(s2)
        (a, s3)
      })
    }

    def >>=[A, B](state: State[S, A], f: A => State[S, B]): State[S, B] = {
      new State[S, B](s => {
        val (a, s2) = state.state(s)
        f(a).state(s2)
      })
    }
  }
}
