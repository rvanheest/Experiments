package monadics.temp

//import monadics.structures.{MonadPlus, MonadTrans2}
//
//import scala.language.{higherKinds, reflectiveCalls}
//
//case class StateT[S, +A, M[+_]](state: S => M[(A, S)])(implicit theState: StateTMonadPlus[S, M], mp: MonadPlus[M]) {
//
//	import StateT.{failure, from}
//
//	def run(s: S): M[(A, S)] = state(s)
//
//	def eval(s: S): M[A] = mp.map(state(s))(_._1)
//
//	def execute(s: S): M[S] = mp.map(state(s))(_._2)
//
//	def orElse[B >: A](other: => StateT[S, B, M]): StateT[S, B, M] = {
//		theState.orElse(this, other)
//	}
//	def <|>[B >: A](other: => StateT[S, B, M]): StateT[S, B, M] = {
//		this.orElse(other)
//	}
//
//	def map[B](f: A => B): StateT[S, B, M] = {
//		theState.map(this)(f)
//	}
//
//	def doOnNext[Ignore](f: A => Ignore): StateT[S, A, M] = {
//		map(a => { f(a); a })
//	}
//
//	def flatMap[B](f: A => StateT[S, B, M]): StateT[S, B, M] = {
//		theState.flatMap(this)(f)
//	}
//	def >>=[B](f: A => StateT[S, B, M]): StateT[S, B, M] = {
//		this.flatMap(f)
//	}
//
//	def >>[B](other: => StateT[S, B, M]): StateT[S, B, M] = {
//		this >>= (_ => other)
//	}
//
//	def <<[B](other: => StateT[S, B, M]): StateT[S, A, M] = {
//		this >>= (x => other >> from(x))
//	}
//
//	def filter(predicate: A => Boolean): StateT[S, A, M] = satisfy(predicate)
//	def satisfy(predicate: A => Boolean): StateT[S, A, M] = {
//		this >>= (x => if (predicate(x)) from(x) else failure)
//	}
//
//	def maybe: StateT[S, Option[A], M] = {
//		map(Option(_)) <|> from(Option.empty)
//	}
//
//	def many: StateT[S, List[A], M] = {
//		atLeastOnce <|> from(Nil)
//	}
//
//	def atLeastOnce: StateT[S, List[A], M] = {
//		for {
//			x <- this
//			xs <- many
//		} yield x :: xs
//	}
//
//	def takeUntil(predicate: A => Boolean): StateT[S, List[A], M] = {
//		takeWhile(!predicate(_))
//	}
//
//	def takeWhile(predicate: A => Boolean): StateT[S, List[A], M] = {
//		satisfy(predicate).many
//	}
//
//	def separatedBy[Sep](sep: StateT[S, Sep, M]): StateT[S, List[A], M] = {
//		separatedBy1(sep) <|> from(Nil)
//	}
//
//	def separatedBy1[Sep](sep: StateT[S, Sep, M]): StateT[S, List[A], M] = {
//		for {
//			x <- this
//			xs <- (sep >> this).many
//		} yield x :: xs
//	}
//
//	def skipMany: StateT[S, Unit, M] = this >> skipMany <|> from(())
//}
//
//object StateT {
//	def from[S, A, M[+_]](a: A)(implicit theState: StateTMonadPlus[S, M], m: MonadPlus[M]): StateT[S, A, M] = {
//		StateT(m.create(a, _))
//	}
//
//	def failure[S, A, M[+_]](implicit theState: StateTMonadPlus[S, M], m: MonadPlus[M]): StateT[S, A, M] = {
//		StateT(_ => m.empty)
//	}
//
//	def failure[S, A, M[+_]](e: Throwable)(implicit theState: StateTMonadPlus[S, M], m: MonadPlus[M]): StateT[S, A, M] = {
//		StateT(_ => m.fail(e))
//	}
//}
//
//package object stateTMonadPlus {
//	type StateTMonadPlus[S, M[+_]] = MonadPlus[({type s[x] = StateT[S, x, M]})#s]
//
//	implicit def stateTIsMonadPlus[S, M[+_]](implicit mp: MonadPlus[M]): StateTMonadPlus[S, M] = new StateTMonadPlus[S, M] {
//		def empty[A]: StateT[S, A, M] = StateT.failure(this, mp)
//
//		def create[A](a: A): StateT[S, A, M] = StateT.from(a)(this, mp)
//
//		def fail[A](e: Throwable): StateT[S, A, M] = StateT.failure(e)(this, mp)
//
//		def orElse[A, B >: A](alt1: StateT[S, A, M], alt2: => StateT[S, B, M]): StateT[S, B, M] = {
//			StateT(s => mp.orElse(alt1.run(s), alt2.run(s)))
//		}
//
//		def map[A, B](functor: StateT[S, A, M])(f: A => B): StateT[S, B, M] = {
//			StateT(s => mp.map(functor.run(s)) { case (a, ss) => (f(a), ss) })
//		}
//
//		def flatMap[A, B](monad: StateT[S, A, M])(f: A => StateT[S, B, M]): StateT[S, B, M] = {
//			StateT(s => mp.flatMap(monad.run(s)) { case (a, ss) => f(a).run(ss) })
//		}
//	}
//
//	type StateTMonadTrans[S] = MonadTrans2[({type s[x[+_], y] = StateT[S, y, x]})#s]
//
////	implicit def stateTIsMonadTrans[S]: StateTMonadTrans[S] = new StateTMonadTrans[S] {
////		def lift[M[+_], A](ma: M[A])(implicit monad: Monad[M]): StateT[S, A, M] = {
////			StateT(s => monad.map(ma)(a => (a, s)))
////		}
////	}
//}
