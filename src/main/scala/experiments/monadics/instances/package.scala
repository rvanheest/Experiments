package experiments.monadics

import scala.language.{implicitConversions, reflectiveCalls}

package object instances {

	implicit def sumIsMonoid[A: Numeric] = new Monoid[Sum[A]] {
		implicit val m: Monoid[Sum[A]] = this

		def empty: Sum[A] = Sum(implicitly[Numeric[A]].zero)

		def append(a1: Sum[A], a2: Sum[A]): Sum[A] = {
			Sum(implicitly[Numeric[A]].plus(a1.sum, a2.sum))
		}
	}

	implicit def productIsMonoid[A: Numeric] = new Monoid[Product[A]] {
		implicit val m: Monoid[Product[A]] = this

		def empty: Product[A] = Product(implicitly[Numeric[A]].zero)

		def append(a1: Product[A], a2: Product[A]): Product[A] = {
			Product(implicitly[Numeric[A]].times(a1.product, a2.product))
		}
	}

	implicit def identityIsMonad = new Monad[Identity] {
		implicit val m: Monad[Identity] = this

		def create[A](a: A): Identity[A] = new Identity[A](a)

		def map[A, B](functor: Identity[A])(f: A => B): Identity[B] = {
			new Identity[B](f(functor.id))
		}

		def flatMap[A, B](monad: Identity[A])(f: (A) => Identity[B]): Identity[B] = {
			f(monad.id)
		}
	}

	implicit def maybeIsMonoid[A: Monoid]: Monoid[Maybe[A]] = new Monoid[Maybe[A]] {
		override def empty: Maybe[A] = Maybe.empty

		override def append(m1: Maybe[A], m2: Maybe[A]): Maybe[A] = {
			(m1, m2) match {
				case (None, m) => m
				case (m, None) => m
				case (Just(a1), Just(a2)) => Just(implicitly[Monoid[A]].append(a1, a2))
			}
		}
	}

	implicit def maybeIsMonadPlus: MonadPlus[Maybe] = new MonadPlus[Maybe] {
		implicit val m: MonadPlus[Maybe] = this

		def create[A](a: A): Maybe[A] = Maybe.apply(a)

		def empty[A]: Maybe[A] = Maybe.empty

		def getOrElse[A, B >: A](alt: Maybe[A], default: => B): B = {
			alt match {
				case Just(a) => a
				case None => default
			}
		}

		def orElse[A, B >: A](alt1: Maybe[A], alt2: => Maybe[B]): Maybe[B] = {
			alt1 match {
				case None => alt2
				case _ => alt1
			}
		}

		def map[A, B](functor: Maybe[A])(f: A => B): Maybe[B] = {
			functor match {
				case Just(a) => Just(f(a))
				case None => None
			}
		}

		def flatMap[A, B](monad: Maybe[A])(f: A => Maybe[B]): Maybe[B] = {
			monad match {
				case Just(a) => f(a)
				case None => None
			}
		}
	}

	implicit def stateIsMonad[S]: Monad[({type s[x] = State[S, x]})#s] = new Monad[({type s[x] = State[S, x]})#s] {

		def create[B](b: B): State[S, B] = new State(s => (b, s))

		def map[A, B](state: State[S, A])(f: A => B): State[S, B] = {
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

		def flatMap[A, B](state: State[S, A])(f: A => State[S, B]): State[S, B] = {
			new State[S, B](s => {
				val (a, s2) = state.state(s)
				f(a).state(s2)
			})
		}
	}

	implicit def functionIsArrow: Arrow[Function] = new Arrow[Function] {
		def id[A]: Function[A, A] = Function(identity)

		def create[A, B](f: A => B): Function[A, B] = {
			Function(f)
		}

		def compose[A, B, C](bc: Function[B, C], ab: Function[A, B]): Function[A, C] = {
			Function(bc.f.compose(ab.f))
		}

		def ***[A, B, C, D](ab: Function[A, B], cd: Function[C, D]): Function[(A, C), (B, D)] = {
			Function { case (a, c) => (ab(a), cd(c)) }
		}
	}

	implicit def functionIsMonad[S]: Monad[({type s[x] = Function[S, x]})#s] = new Monad[({type s[x] = Function[S, x]})#s] {
		def map[A, B](functor: Function[S, A])(f: (A) => B): Function[S, B] = {
			Function(f compose functor.apply)
		}

		def create[A](a: A): Function[S, A] = {
			Function(_ => a)
		}

		def flatMap[A, B](monad: Function[S, A])(f: (A) => Function[S, B]): Function[S, B] = {
			Function(s => f(monad(s))(s))
		}
	}
}
