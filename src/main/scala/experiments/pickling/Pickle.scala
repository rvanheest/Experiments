package experiments.pickling

import experiments.parsec.Parser

import scala.language.{ higherKinds, implicitConversions, reflectiveCalls }
import scala.reflect.{ ClassTag, classTag }
import scala.util.{ Failure, Success, Try }

trait PickleBuilder[A, State, Repr] {
	def apply(pickle: (A, State) => Try[State], unpickle: State => (Try[A], State)): Repr
}

// TODO improve error messages
// TODO look into whether or not we need things like `wrap`, `pair`, etc. now that `seq` can produce a monad
abstract class Pickle[A, State](val pickle: (A, State) => Try[State],
																val unpickle: State => (Try[A], State)) {

	import Pickle.unpickleAsParser

	type Repr[X] <: Pickle[X, State]

	protected[this] implicit def builder[X]: PickleBuilder[X, State, Repr[X]]

	def seq: SeqBuilder[A, A] = new SeqBuilder(this, identity)

	def seq[B](f: B => A): SeqBuilder[A, B] = new SeqBuilder(this, f)

	def upcast[B >: A](implicit ctA: ClassTag[A], ctB: ClassTag[B]): Repr[B] = {
		this.seq[B] {
			case a: A => a
			case _ => sys.error(s"can't cast ${ classTag[B] } to ${ classTag[A] }")
		}.map(identity)
	}

	def orElse(other: => Pickle[A, State]): Repr[A] = {
		builder(
			pickle = (a, state) => this.pickle(a, state) orElse other.pickle(a, state),
			unpickle = (this.unpickle <|> other.unpickle).run)
	}

	def satisfy(predicate: A => Boolean): Repr[A] = this.seq.filter(predicate)

	def filter(predicate: A => Boolean): Repr[A] = this.satisfy(predicate)

	def noneOf(as: List[A]): Repr[A] = {
		satisfy(!as.contains(_))
	}

	def maybe: Repr[Option[A]] = {
		builder(
			pickle = (optA, state) => optA.map(this.pickle(_, state)).getOrElse(Try(state)),
			unpickle = this.unpickle.maybe.run)
	}

	def many: Repr[List[A]] = {
		builder(
			pickle = (as, state) => as.foldRight(Try(state))((a, triedState) => triedState.flatMap(this.pickle(a, _))),
			unpickle = this.unpickle.many.run)
	}

	def atLeastOnce: Repr[List[A]] = {
		for {
			x <- this.seq[List[A]](_.head)
			xs <- many.seq[List[A]](_.tail)
		} yield x :: xs
	}

	def takeUntil(predicate: A => Boolean): Repr[List[A]] = {
		takeWhile(!predicate(_))
	}

	def takeWhile(predicate: A => Boolean): Repr[List[A]] = {
		builder(
			pickle = this.satisfy(predicate).many.pickle,
			unpickle = this.unpickle.takeWhile(predicate).run)
	}

	def separatedBy[Sep](separator: Sep)(sep: Repr[Sep]): Repr[List[A]] = {
		builder(
			pickle = this.separatedBy1(separator)(sep).orElse(Pickle.lift[List[A], State, Repr](Nil)).pickle,
			unpickle = this.unpickle.separatedBy(Parser(sep.unpickle)).run)
	}

	def separatedBy1[Sep](separator: Sep)(sep: Repr[Sep]): Repr[List[A]] = {
		for {
			x <- this.seq[List[A]](_.head)
			xs <- sep.seq[A](_ => separator).flatMap(_ => this).many.seq[List[A]](_.tail)
		} yield x :: xs
	}

	class SeqBuilder[X, Y](pickleA: Pickle[X, State], f: Y => X) {
		def map(g: X => Y): Repr[Y] = {
			builder(
				pickle = (y, state) => {
					for {
						x <- Try { f(y) }
						state2 <- pickleA.pickle(x, state)
					} yield state2
				},
				unpickle = pickleA.unpickle.map(g).run)
		}

		def flatMap(g: X => Pickle[Y, State]): Repr[Y] = {
			builder(
				pickle = (y, state) => {
					for {
						x <- Try { f(y) }
						state2 <- g(x).pickle(y, state)
						state3 <- pickleA.pickle(x, state2)
					} yield state3
				},
				unpickle = pickleA.unpickle.flatMap(g(_).unpickle).run)
		}

		def filter(predicate: X => Boolean): Repr[X] = {
			builder(
				pickle = (x, state) => if (predicate(x)) Success(state)
															 else Failure(new NoSuchElementException("empty pickle")),
				unpickle = pickleA.unpickle.satisfy(predicate).run)
		}
	}
}

object Pickle {

	private[pickling] implicit def unpickleAsParser[A, State](f: State => (Try[A], State)): Parser[State, A] = Parser(f)

	def lift[A, State, Repr[X] <: Pickle[X, State]](a: A)(implicit builder: PickleBuilder[A, State, Repr[A]]): Repr[A] = {
		builder(
			pickle = (_, s) => Try(s),
			unpickle = Parser.from(a).run)
	}

	def empty[A, State, Repr[X] <: Pickle[X, State]](implicit builder: PickleBuilder[A, State, Repr[A]]): Repr[A] = {
		builder(
			pickle = (_, _) => Failure(new NoSuchElementException("empty pickle")),
			unpickle = Parser.empty.run)
	}

	def alt[A, State, Repr[X] <: Pickle[X, State]](as: Array[Pickle[A, State]])(selector: A => Int)(implicit builder: PickleBuilder[A, State, Repr[A]]): Repr[A] = {
		builder(
			pickle = (a, state) => as(selector(a)).pickle(a, state),
			unpickle = as.view
			  .map(p => Parser(p.unpickle))
			  .reduceOption(_ <|> _)
				.getOrElse(Parser.empty)
				.run)
	}
}
