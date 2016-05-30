package experiments.transformers

import experiments.monadics.MonadPlus

import scala.language.{higherKinds, reflectiveCalls}

class StateT[S, A, M[_]](val state: S => M[(A, S)])(implicit m: MonadPlus[M]) {

  import StateT.{failure, from, lift}

  def run(s: S): M[(A, S)] = state(s)

  def eval(s: S): M[A] = m.map(state(s))(_._1)

  def execute(s: S): M[S] = m.map(state(s))(_._2)

  def map[B](f: A => B): StateT[S, B, M] = {
    lift(st => m.map(state(st)) { case (a, s) => (f(a), s) })
  }

  def flatMap[B](f: A => StateT[S, B, M]): StateT[S, B, M] = this >>= f
  def >>=[B](f: A => StateT[S, B, M]): StateT[S, B, M] = {
    lift(st => m.flatMap(state(st)) { case (a, s) => f(a).run(s) })
  }

  def >>[B](other: => StateT[S, B, M]): StateT[S, B, M] = {
    >>= (_ => other)
  }

  def <<[B](other: => StateT[S, B, M]): StateT[S, A, M] = {
    >>= (x => other >>= (_ => from(x)))
  }

  def orElse[B >: A](other: => StateT[S, B, M]): StateT[S, B, M] = this <|> other
  def <|>[B >: A](other: => StateT[S, B, M]): StateT[S, B, M] = {
    lift(st => m.orElse(run(st), other.run(st)))
  }

  def filter(predicate: A => Boolean): StateT[S, A, M] = satisfy(predicate)
  def satisfy(predicate: A => Boolean): StateT[S, A, M] = {
    >>= (x => if (predicate(x)) from(x) else failure)
  }

  def noneOf(as: List[A]): StateT[S, A, M] = {
    satisfy (!as.contains(_))
  }

  def maybe: StateT[S, Option[A], M] = map(Option(_)) <|> from(Option.empty)

  def many: StateT[S, List[A], M] = atLeastOnce <|> from(Nil)

  def atLeastOnce: StateT[S, List[A], M] = {
    >>= (v => many map (v :: _))
  }

  def takeUntil(predicate: A => Boolean): StateT[S, List[A], M] = {
    takeWhile(!predicate(_))
  }

  def takeWhile(predicate: A => Boolean): StateT[S, List[A], M] = {
    satisfy(predicate).many
  }

  def separatedBy[Sep](sep: StateT[S, Sep, M]): StateT[S, List[A], M] = {
    separatedBy1(sep) <|> from(Nil)
  }

  def separatedBy1[Sep](sep: StateT[S, Sep, M]): StateT[S, List[A], M] = {
    >>= (x => (sep >> this).many.map(x :: _))
  }

  def skipMany: StateT[S, Unit, M] = this >> skipMany <|> from(())
}
object StateT {
  def lift[S, A, M[_]](x: S => M[(A, S)])(implicit m: MonadPlus[M]): StateT[S, A, M] = {
    new StateT[S, A, M](x)
  }

  def from[S, A, M[_]](a: A)(implicit m: MonadPlus[M]): StateT[S, A, M] = {
    new StateT[S, A, M](s => m.create((a, s)))
  }

  def failure[S, A, M[_]](implicit m: MonadPlus[M]): StateT[S, A, M] = {
    new StateT(_ => m.empty)
  }
}
