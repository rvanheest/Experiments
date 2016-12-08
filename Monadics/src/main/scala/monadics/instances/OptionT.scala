package monadics.instances

import monadics.structures.{Equals, Monad, MonadPlus, MonadTrans}

import scala.language.{higherKinds, reflectiveCalls}

class OptionT[M[+_], A](trans: M[Option[A]])(implicit m: MonadPlus[OptionT[M, ?]]) {

	def get: M[Option[A]] = trans

	def map[B](f: A => B): OptionT[M, B] = {
		m.map(this)(f)
	}

	def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] = {
		m.flatMap(this)(f)
	}

	def filter(predicate: A => Boolean): OptionT[M, A] = {
		m.filter(this)(predicate)
	}
}

object OptionT {
	type OptionTMonadTrans = MonadTrans[Lambda[(`X[+_]`, Y) => OptionT[X, Y]]]

	def empty[M[+_], A](implicit monad: MonadPlus[OptionT[M, ?]]): OptionT[M, A] = {
		monad.empty
	}

	def create[M[+_], A](a: A)(implicit monad: MonadPlus[OptionT[M, ?]]): OptionT[M, A] = {
		monad.create(a)
	}

	def lift[M[+_], A](ma: M[A])(implicit trans: OptionTMonadTrans, monad: Monad[M]): OptionT[M, A] = {
		trans.lift(ma)
	}

	def apply[M[+_], A](trans: M[Option[A]])(implicit monad: MonadPlus[OptionT[M, ?]]): OptionT[M, A] = {
		new OptionT(trans)
	}

	implicit def optionTIsEquals[M[+_], A](implicit mEquals: Equals[M[Option[A]]]): Equals[OptionT[M, A]] = {
		Equals.create((o1, o2) => mEquals.equals(o1.get, o2.get))
	}

	implicit def optionTIsMonadPlus[M[+_]](implicit monad: Monad[M]): MonadPlus[OptionT[M, ?]] = new MonadPlus[OptionT[M, ?]] {
		def empty[A]: OptionT[M, A] = new OptionT(monad.create(Option.empty))

		def create[A](a: A): OptionT[M, A] = new OptionT(monad.create(Option(a)))

		override def map[A, B](optionT: OptionT[M, A])(f: A => B): OptionT[M, B] = {
			new OptionT(monad.map(optionT.get)(_.map(f)))
		}

		def flatMap[A, B](optionT: OptionT[M, A])(f: A => OptionT[M, B]): OptionT[M, B] = {
			new OptionT(monad.flatMap(optionT.get)(_.map(f(_).get).getOrElse(monad.create(Option.empty))))
		}

		def orElse[A, B >: A](alt1: OptionT[M, A], alt2: => OptionT[M, B]): OptionT[M, B] = {
			new OptionT(monad.flatMap(alt1.get)(optA => optA.map(_ => monad.create(optA)).getOrElse(alt2.get)))
		}
	}

	implicit val optionTIsMonadTrans: OptionTMonadTrans = new OptionTMonadTrans {
		def lift[M[+_], A](ma: M[A])(implicit monad: Monad[M]): OptionT[M, A] = {
			new OptionT(monad.map(ma)(Option(_)))
		}
	}
}
