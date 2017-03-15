package experiments.pickling

import experiments.parsec.Parser

import scala.language.{ higherKinds, implicitConversions, reflectiveCalls }
import scala.reflect.{ ClassTag, classTag }
import scala.util.Try

trait PickleBuilder[A, State, Repr] {
	def apply(pickle: (A, State) => State, unpickle: Parser[State, A]): Repr
}

abstract class Pickle[A, State](val pickle: (A, State) => State,
																val unpickle: Parser[State, A]) {

	type Repr[X] <: Pickle[X, State]

	protected[this] def builder[X]: PickleBuilder[X, State, Repr[X]]

	def maybe: Repr[Option[A]] = {
		builder(
			pickle = (optA: Option[A], state: State) => optA.map(this.pickle(_, state)).getOrElse(state),
			unpickle = this.unpickle.maybe)
	}

	def orElse(other: => Pickle[A, State]): Repr[A] = {
		builder(
			pickle = (a: A, state: State) => {
				Try { this.pickle(a, state) }
				  .getOrElse(other.pickle(a, state))
			},
			unpickle = this.unpickle <|> other.unpickle
		)
	}

	// TODO many, etc; see parser operators

	def seq[B](f: B => A): SeqBuilder[B] = new SeqBuilder(this, f)

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

	def wrap[B: ClassTag](f: A => B): WrapBuilder[B] = new WrapBuilder(this, f)

	class SeqBuilder[B](pickleA: Pickle[A, State], f: B => A) {
		def map(g: A => B): Repr[B] = {
			builder(
				pickle = (b, state) => pickleA.pickle(f(b), state),
				unpickle = pickleA.unpickle.map(g))
		}

		def flatMap(g: A => Pickle[B, State]): Repr[B] = {
			builder(
				pickle = (b, state) => {
					val a = f(b)
					pickleA.pickle(a, g(a).pickle(b, state))
				},
				unpickle = pickleA.unpickle.flatMap(g(_).unpickle))
		}
	}

	class WrapBuilder[B: ClassTag](pickleA: Pickle[A, State], f: A => B) {
		def unwrap(g: PartialFunction[B, A]): Repr[B] = {
			new SeqBuilder[B](pickleA, b => if (g isDefinedAt b) g(b)
																			else sys.error(s"undefined unwrapper for ${classTag[B]}")).map(f)
		}
	}
}

object Pickle {
//	@inline implicit def pickleWrapper[A, State](pickle: Pickle[A, State]): RichPickle[A, State] = {
//		new RichPickle(pickle)
//	}

	def lift[A, State, Repr[X] <: Pickle[X, State]](a: A)(implicit builder: PickleBuilder[A, State, Repr[A]]): Repr[A] = {
		builder(
			pickle = (_, s) => s,
			unpickle = Parser.from(a))
	}

	def alt[A, State, Repr[X] <: Pickle[X, State]](as: Array[Pickle[A, State]])(selector: A => Int)(implicit builder: PickleBuilder[A, State, Repr[A]]): Repr[A] = {
		builder(
			pickle = (a, state) => as(selector(a)).pickle(a, state),
			unpickle = as.view.map(_.unpickle).reduceOption(_ <|> _).getOrElse(Parser.empty))
	}
}
