package experiments.transformers

import experiments.monadics.MonadPlus

import scala.util.{Failure, Try}

case class TryT[M[_], A](run: M[Try[A]])(implicit m: MonadPlus[M]) {
	import TryT.lift

	def map[B](f: A => B): TryT[M, B] = {
		lift(m.map(run)(_ map f))
	}

	def flatMap[B](f: A => TryT[M, B]): TryT[M, B] = {
		lift(m.flatMap(run)(_ map f.andThen(_.run) getOrElse
			m.create(Failure(new NoSuchElementException("Predicate does not hold for " + run)))))
	}

	def filter(predicate: A => Boolean): TryT[M, A] = {
		lift(m.filter(run)(_ filter predicate map (_ => true) getOrElse false))
	}
}
object TryT {
	def lift[M[_], A](tt: M[Try[A]])(implicit m: MonadPlus[M]): TryT[M, A] = new TryT(tt)
}
