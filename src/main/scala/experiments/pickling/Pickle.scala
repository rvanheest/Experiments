package experiments.pickling

import experiments.parsec.Parser

import scala.language.{ higherKinds, implicitConversions, reflectiveCalls }
import scala.reflect.{ ClassTag, classTag }
import scala.util.{ Failure, Try }

trait PickleBuilder[A, State, Repr] {
	def apply(pickle: (A, State) => Try[State], unpickle: Parser[State, A]): Repr
}

// TODO improve error messages
// TODO hide the parser in unpickle
// TODO look into whether or not we need things like `wrap`, `pair`, etc. now that `seq` can produce a monad
abstract class Pickle[A, State](val pickle: (A, State) => Try[State],
																val unpickle: Parser[State, A]) {

	type Repr[X] <: Pickle[X, State]

	protected[this] implicit def builder[X]: PickleBuilder[X, State, Repr[X]]

	def seq[B](f: B => A): SeqBuilder[A, B] = new SeqBuilder(this, f)

	def pair[B](pb: Pickle[B, State]): Repr[(A, B)] = {
		for {
			a <- this.seq[(A, B)]({ case (a, _) => a })
			b <- pb.seq[(A, B)]({ case (_, b) => b })
		} yield (a, b)
	}

	def triple[B, C](pb: Pickle[B, State], pc: Pickle[C, State]): Repr[(A, B, C)] = {
		for {
			a <- this.seq[(A, B, C)]({ case (a, _, _) => a })
			b <- pb.seq[(A, B, C)]({ case (_, b, _) => b })
			c <- pc.seq[(A, B, C)]({ case (_, _, c) => c })
		} yield (a, b, c)
	}

	def quad[B, C, D](pb: Pickle[B, State], pc: Pickle[C, State], pd: Pickle[D, State]): Repr[(A, B, C, D)] = {
		for {
			a <- this.seq[(A, B, C, D)]({ case (a, _, _, _) => a })
			b <- pb.seq[(A, B, C, D)]({ case (_, b, _, _) => b })
			c <- pc.seq[(A, B, C, D)]({ case (_, _, c, _) => c })
			d <- pd.seq[(A, B, C, D)]({ case (_, _, _, d) => d })
		} yield (a, b, c, d)
	}

	def wrap[B: ClassTag](f: A => B): WrapBuilder[A, B] = new WrapBuilder(this, f)

	def orElse(other: => Pickle[A, State]): Repr[A] = {
		builder(
			pickle = (a, state) => this.pickle(a, state) orElse other.pickle(a, state),
			unpickle = this.unpickle <|> other.unpickle)
	}

	def satisfy(predicate: A => Boolean): Repr[A] = {
		builder(
			pickle = this.seq[A](identity)
				.flatMap(a => if (predicate(a)) Pickle.lift[A, State, Repr](a)
											else Pickle.empty[A, State, Repr])
				.pickle,
			unpickle = this.unpickle.satisfy(predicate))
	}

	def noneOf(as: List[A]): Repr[A] = {
		satisfy(!as.contains(_))
	}

	def maybe: Repr[Option[A]] = {
		builder(
			pickle = (optA, state) => optA.map(this.pickle(_, state)).getOrElse(Try(state)),
			unpickle = this.unpickle.maybe)
	}

	def many: Repr[List[A]] = {
		builder(
			pickle = (as, state) => as.foldRight(Try(state))((a, triedState) => triedState.flatMap(this.pickle(a, _))),
			unpickle = this.unpickle.many)
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
			unpickle = this.unpickle.takeWhile(predicate))
	}

	def separatedBy[Sep](separator: Sep)(sep: Repr[Sep]): Repr[List[A]] = {
		builder(
			pickle = this.separatedBy1(separator)(sep).orElse(Pickle.lift[List[A], State, Repr](Nil)).pickle,
			unpickle = this.unpickle.separatedBy(sep.unpickle))
	}

	def separatedBy1[Sep](separator: Sep)(sep: Repr[Sep]): Repr[List[A]] = {
		for {
			x <- this.seq[List[A]](_.head)
			xs <- sep.seq[A](_ => separator).flatMap(_ => this).many.seq[List[A]](_.tail)
		} yield x :: xs
	}

	class SeqBuilder[A2, B](pickleA: Pickle[A2, State], f: B => A2) {
		def map(g: A2 => B): Repr[B] = {
			builder(
				pickle = (b, state) => {
					for {
						a <- Try { f(b) }
						state2 <- pickleA.pickle(a, state)
					} yield state2
				},
				unpickle = pickleA.unpickle.map(g))
		}

		def flatMap(g: A2 => Pickle[B, State]): Repr[B] = {
			builder(
				pickle = (b, state) => {
					for {
						a <- Try { f(b) }
						state2 <- g(a).pickle(b, state)
						state3 <- pickleA.pickle(a, state2)
					} yield state3
				},
				unpickle = pickleA.unpickle.flatMap(g(_).unpickle))
		}
	}

	class WrapBuilder[A2, B: ClassTag](pickleA: Pickle[A2, State], f: A2 => B) {
		def unwrap(g: PartialFunction[B, A2]): Repr[B] = {
			// TODO replace with .seq call???
			new SeqBuilder[A2, B](pickleA, b => if (g isDefinedAt b) g(b)
																					else sys.error(s"undefined unwrapper for ${classTag[B]}"))
				.map(f)
		}
	}
}

object Pickle {

	def lift[A, State, Repr[X] <: Pickle[X, State]](a: A)(implicit builder: PickleBuilder[A, State, Repr[A]]): Repr[A] = {
		builder(
			pickle = (_, s) => Try(s),
			unpickle = Parser.from(a))
	}

	def empty[A, State, Repr[X] <: Pickle[X, State]](implicit builder: PickleBuilder[A, State, Repr[A]]): Repr[A] = {
		builder(
			pickle = (_, _) => Failure(new NoSuchElementException("empty pickle")),
			unpickle = Parser.empty
		)
	}

	def alt[A, State, Repr[X] <: Pickle[X, State]](as: Array[Pickle[A, State]])(selector: A => Int)(implicit builder: PickleBuilder[A, State, Repr[A]]): Repr[A] = {
		builder(
			pickle = (a, state) => as(selector(a)).pickle(a, state),
			unpickle = as.view.map(_.unpickle).reduceOption(_ <|> _).getOrElse(Parser.empty))
	}
}
