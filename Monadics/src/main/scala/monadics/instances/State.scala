package monadics.instances

import monadics.structures.{Equals, Monad}

import scala.language.reflectiveCalls

class State[S, A](state: S => (A, S))(implicit monad: Monad[State[S, ?]]) {

	def run(s: S): (A, S) = state(s)

	def evaluate(s: S): A = state(s)._1

	def execute(s: S): S = state(s)._2

	def map[B](f: A => B): State[S, B] = monad.map(this)(f)

	def as[B](b: => B): State[S, B] = monad.as(this, b)

	def void: State[S, Unit] = monad.void(this)

	def zipWith[B](f: A => B): State[S, (A, B)] = monad.zipWith(this)(f)

	def <*>[B, C](other: State[S, B])(implicit ev: State[S, A] <:< State[S, (B => C)]): State[S, C] = monad.<*>(this, other)

	def *>[B](other: State[S, B]): State[S, B] = monad.*>(this, other)

	def <*[B](other: State[S, B]): State[S, A] = monad.<*(this, other)

	def <**>[B](other: State[S, A => B]): State[S, B] = monad.<**>(this, other)

	def flatMap[B](f: A => State[S, B]): State[S, B] = monad.flatMap(this)(f)

	def andThen[B](other: State[S, B]): State[S, B] = monad.andThen(this, other)

	def thenAnd[B](other: State[S, B]): State[S, A] = monad.thenAnd(this, other)

	def flatten[B](implicit ev: A <:< State[S, B]): State[S, B] = monad.flatten(this)(ev)
}

object State {
	def get[S]: State[S, S] = new State(s => (s, s))

	def put[S](newState: S): State[S, Unit] = new State(_ => ((), newState))

	def apply[S, A](state: S => (A, S))(implicit monad: Monad[State[S, ?]]): State[S, A] = new State(state)

	implicit def stateIsEquals[S, A](implicit fEquals: Equals[S => (A, S)]): Equals[State[S, A]] = {
		Equals.create((s1, s2) => fEquals.equals(s1.run, s2.run))
	}

	implicit def stateIsMonad[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {
		def create[B](b: B): State[S, B] = new State(s => (b, s))

		override def map[A, B](state: State[S, A])(f: A => B): State[S, B] = {
			State[S, B](s => {
				val (a, s2) = state.run(s)
				(f(a), s2)
			})
		}

		override def <*>[A, B](stateAB: State[S, A => B], stateA: State[S, A]): State[S, B] = {
			State[S, B](s => {
				val (aToB, s2) = stateAB.run(s)
				val (a, s3) = stateA.run(s2)
				(aToB(a), s3)
			})
		}

		override def *>[A, B](stateA: State[S, A], stateB: State[S, B]): State[S, B] = {
			State[S, B](s => {
				val (_, s2) = stateA.run(s)
				stateB.run(s2)
			})
		}

		override def <*[A, B](stateA: State[S, A], stateB: State[S, B]): State[S, A] = {
			State[S, A](s => {
				val (a, s2) = stateA.run(s)
				val (_, s3) = stateB.run(s2)
				(a, s3)
			})
		}

		def flatMap[A, B](state: State[S, A])(f: A => State[S, B]): State[S, B] = {
			State[S, B](s => {
				val (a, s2) = state.run(s)
				f(a).run(s2)
			})
		}
	}
}
