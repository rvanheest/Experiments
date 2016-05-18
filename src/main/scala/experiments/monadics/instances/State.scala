package experiments.monadics.instances

import experiments.monadics.Monad

import scala.language.reflectiveCalls

class State[S, A](val state: S => (A, S))(implicit m: Monad[({ type s[x] = State[S, x] })#s]) {

  def run(s: S): (A, S) = state(s)

  def evaluate(s: S): A = state(s)._1

  def execute(s: S): S = state(s)._2

  def map[B](f: A => B): State[S, B] = m.map(this)(f)

  def <*>[B, C](other: State[S, B])(implicit ev: A <:< (B => C)): State[S, C] = {
    m.<*>(this.map(ev), other)
  }

  def <**>[B](other: State[S, A => B]): State[S, B] = m.<**>(this, other)

  def flatMap[B](f: A => State[S, B]): State[S, B] = m.flatMap(this)(f)
  def >>=[B](f: A => State[S, B]): State[S, B] = flatMap(f)

  def andThen[B](other: State[S, B]): State[S, B] = m.andThen(this, other)
  def >>[B](other: State[S, B]): State[S, B] = andThen(other)

  def thenAnd[B](other: State[S, B]): State[S, A] = m.thenAnd(this, other)
  def <<[B](other: State[S, B]): State[S, A] = thenAnd(other)

  def flatten[B](implicit ev: A <:< State[S, B]): State[S, B] = m.flatten(this)(ev)
}

object State {
  def get[S]: State[S, S] = new State(s => (s, s))

  def put[S](newState: S): State[S, Unit] = new State(_ => ((), newState))
}
