package monadics.instances

import monadics.structures._

import scala.language.{higherKinds, reflectiveCalls}

class StateT[S, A, M[+_]](state: S => M[(A, S)])(implicit theState: MonadPlus[StateT[S, ?, M]], mp: Monad[M]) {

	def run(s: S): M[(A, S)] = state(s)

	def eval(s: S): M[A] = mp.map(state(s))(_._1)

	def execute(s: S): M[S] = mp.map(state(s))(_._2)

	def orElse[B >: A](other: => StateT[S, B, M]): StateT[S, B, M] = {
		theState.combine(this, other)
	}
	def <|>[B >: A](other: => StateT[S, B, M]): StateT[S, B, M] = this.orElse(other)

	def map[B](f: A => B): StateT[S, B, M] = {
		theState.map(this)(f)
	}

	def doOnNext[Ignore](f: A => Ignore): StateT[S, A, M] = {
		this.map(a => {
			f(a)
			a
		})
	}

	def flatMap[B](f: A => StateT[S, B, M]): StateT[S, B, M] = {
		theState.flatMap(this)(f)
	}
	def >>=[B](f: A => StateT[S, B, M]): StateT[S, B, M] = this.flatMap(f)

	def >>[B](other: => StateT[S, B, M]): StateT[S, B, M] = {
		theState.andThen(this, other)
	}

	def <<[B](other: => StateT[S, B, M]): StateT[S, A, M] = {
		theState.thenAnd(this, other)
	}

	def filter(predicate: A => Boolean): StateT[S, A, M] = {
		theState.filter(this)(predicate)
	}
	def satisfy(predicate: A => Boolean): StateT[S, A, M] = this.filter(predicate)

	def maybe: StateT[S, Option[A], M] = {
		theState.maybe(this)
	}

	def many: StateT[S, List[A], M] = {
		theState.many(this)
	}

	def atLeastOnce: StateT[S, List[A], M] = {
		theState.atLeastOnce(this)
	}

	def takeUntil(predicate: A => Boolean): StateT[S, List[A], M] = {
		theState.takeWhile(this)(predicate)
	}

	def takeWhile(predicate: A => Boolean): StateT[S, List[A], M] = {
		theState.takeWhile(this)(predicate)
	}

	def separatedBy[Sep](sep: StateT[S, Sep, M]): StateT[S, List[A], M] = {
		this.separatedBy1(sep) <|> StateT.from(Nil)
	}

	def separatedBy1[Sep](sep: StateT[S, Sep, M]): StateT[S, List[A], M] = {
		for {
			x <- this
			xs <- (sep >> this).many
		} yield x :: xs
	}

	def skipMany: StateT[S, Unit, M] = {
		this >> skipMany <|> StateT.from(())
	}
}

object StateT {
	type StateTMonadTrans[S] = MonadTrans[Lambda[(`x[+_]`, y) => StateT[S, y, x]]]

	def empty[S, A, M[+_]](implicit monad: MonadPlus[StateT[S, ?, M]]): StateT[S, A, M] = {
		monad.empty
	}

	def failure[S, A, M[+_]](e: Throwable)(implicit monad: MonadFail[StateT[S, ?, M]]): StateT[S, A, M] = {
		monad.fail(e)
	}

	def from[S, A, M[+_]](a: A)(implicit monad: MonadPlus[StateT[S, ?, M]]): StateT[S, A, M] = {
		monad.create(a)
	}

	def lift[S, A, M[+_]](ma: M[A])(implicit theState: MonadPlus[StateT[S, ?, M]], m: Monad[M]): StateT[S, A, M] = {
		new StateT(s => m.map(ma)(a => (a, s)))
	}

	def apply[S, A, M[+_]](state: S => M[(A, S)])(implicit theState: MonadPlus[StateT[S, ?, M]], mp: Monad[M]): StateT[S, A, M] = {
		new StateT(state)
	}

	implicit def stateTIsEquals[S, A, M[+_]](implicit fEquals: Equals[S => M[(A, S)]]): Equals[StateT[S, A, M]] = {
		Equals.create((s1, s2) => fEquals.equals(s1.run, s2.run))
	}

	implicit def stateTIsMonadPlus[S, M[+_]](implicit mp: MonadPlus[M] with MonadFail[M]): MonadPlus[StateT[S, ?, M]] with MonadFail[StateT[S, ?, M]] = new MonadPlus[StateT[S, ?, M]] with MonadFail[StateT[S, ?, M]] {
		def empty[A]: StateT[S, A, M] = new StateT(_ => mp.empty)

		def fail[A](e: Throwable): StateT[S, A, M] = new StateT[S, A, M](_ => mp.fail(e))

		def create[A](a: A): StateT[S, A, M] = new StateT(mp.create(a, _))

		override def map[A, B](functor: StateT[S, A, M])(f: A => B): StateT[S, B, M] = {
			new StateT(s => mp.map(functor.run(s)) { case (a, ss) => (f(a), ss) })
		}

		def flatMap[A, B](monad: StateT[S, A, M])(f: A => StateT[S, B, M]): StateT[S, B, M] = {
			new StateT(s => mp.flatMap(monad.run(s)) { case (a, ss) => f(a).run(ss) })
		}

		def combine[A, B >: A](alt1: StateT[S, A, M], alt2: => StateT[S, B, M]): StateT[S, B, M] = {
			new StateT(s => mp.combine(alt1.run(s), alt2.run(s)))
		}
	}

//	implicit def stateTIsMonadTrans[S]: StateTMonadTrans[S] = new StateTMonadTrans[S] {
//
//		def lift[M[+ _], A](ma: M[A])(implicit monad: Monad[M]): StateT[S, A, M] = {
//			implicit val mp: StateTMonadPlus[S, M] = stateTIsMonadPlus[S, M]
//			new StateT(s => monad.map(ma)(a => (a, s)))
//		}
//	}
}
