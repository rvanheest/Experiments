package experiments.monadics

import scala.language.reflectiveCalls

package object instances {

  implicit def maybeIsMonoid[T] = new Monoid[T, Maybe] {
    def mappend(monoid: Maybe[T], other: Maybe[T]): Maybe[T] = {
      monoid match {
        case Just(x) => Just(x)
        case Nothing() => other
      }
    }
  }

  implicit def maybeIsMonad[A] = new MonadPlus[A, Maybe] {
    def fmap[B](maybe: Maybe[A], f: A => B): Maybe[B] = {
      maybe match {
        case Just(a) => Just(f(a))
        case Nothing() => Nothing()
      }
    }

    def flatMap[B](maybe: Maybe[A], f: A => Maybe[B]): Maybe[B] = {
      maybe match {
        case Just(a) => f(a)
        case Nothing() => Nothing()
      }
    }

    def mplus(maybe: Maybe[A], other: Maybe[A]): Maybe[A] = maybe.mappend(other)
  }

  implicit def stateIsMonad[A, S] = new Monad[A, ({ type s[x] = State[S, x] })#s] {
    override def fmap[B](state: State[S, A], f: A => B): State[S, B] = {
      new State[S, B](s => {
        val (a, s2) = state.state(s)
        (f(a), s2)
      })
    }

    override def flatMap[B](state: State[S, A], f: A => State[S, B]): State[S, B] = {
      new State[S, B](s => {
        val (a, s2) = state.state(s)
        f(a).state(s2)
      })
    }
  }
}
