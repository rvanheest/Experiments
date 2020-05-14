package experiments.transformers

import experiments.monadics.MonadPlus

import scala.language.{higherKinds, reflectiveCalls}

class StateT[S, A, M[_]](val state: S => M[(A, S)])(implicit m: MonadPlus[M]) {

  import StateT.{failure, from, lift}

  def run(s: S): M[(A, S)] = state(s)

  def eval(s: S): M[A] = m.map(state(s))(_._1)

  def execute(s: S): M[S] = m.map(state(s))(_._2)

  def orElse[B >: A](other: => StateT[S, B, M]): StateT[S, B, M] = this <|> other
  def <|>[B >: A](other: => StateT[S, B, M]): StateT[S, B, M] = {
    lift(st => m.orElse(run(st), other.run(st)))
  }

  def map[B](f: A => B): StateT[S, B, M] = {
    lift(st => m.map(state(st)) { case (a, s) => (f(a), s) })
  }

  def doOnNext[Ignore](f: A => Ignore): StateT[S, A, M] = {
    map(a => { f(a); a })
  }

  def flatMap[B](f: A => StateT[S, B, M]): StateT[S, B, M] = this >>= f
  def >>=[B](f: A => StateT[S, B, M]): StateT[S, B, M] = {
    lift(st => m.flatMap(state(st)) { case (a, s) => f(a).run(s) })
  }

  def >>[B](other: => StateT[S, B, M]): StateT[S, B, M] = {
    this >>= (_ => other)
  }

  def <<[B](other: => StateT[S, B, M]): StateT[S, A, M] = {
    this >>= (x => other >> from(x))
  }

  def filter(predicate: A => Boolean): StateT[S, A, M] = satisfy(predicate)
  def satisfy(predicate: A => Boolean): StateT[S, A, M] = {
    this >>= (x => if (predicate(x)) from(x) else failure)
  }

  def maybe: StateT[S, Option[A], M] = map(Option(_)) <|> from(Option.empty)

  def many: StateT[S, List[A], M] = atLeastOnce <|> from(Nil)

  def atLeastOnce: StateT[S, List[A], M] = {
    for {
      x <- this
      xs <- many
    } yield x :: xs
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
    for {
      x <- this
      xs <- (sep >> this).many
    } yield x :: xs
  }

  def skipMany: StateT[S, Unit, M] = this >> skipMany <|> from(())
}
object StateT {
  def lift[S, A, M[_]](x: S => M[(A, S)])(implicit m: MonadPlus[M]): StateT[S, A, M] = {
    new StateT(x)
  }

  def apply[S, A, M[_]](x: S => M[(A, S)])(implicit m: MonadPlus[M]): StateT[S, A, M] = {
    new StateT(x)
  }

  def from[S, A, M[_]](a: A)(implicit m: MonadPlus[M]): StateT[S, A, M] = {
    StateT(s => m.create((a, s)))
  }

  def failure[S, A, M[_]](implicit m: MonadPlus[M]): StateT[S, A, M] = {
    StateT(_ => m.empty)
  }
}
