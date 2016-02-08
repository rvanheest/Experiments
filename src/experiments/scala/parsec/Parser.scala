package experiments.scala.parsec

import Parser.{failure, from}

class Parser[S, A](parser: S => Option[(A, S)]) {

	def parse(input: S): Option[(A, S)] = parser(input)

	def run(input: S): A = {
		parser(input).map { case (result, _) => result }.getOrElse(sys.error("No result"))
	}

	def orElse[B >: A](other: => Parser[S, B]): Parser[S, B] = this <|> other
	def <|>[B >: A](other: => Parser[S, B]): Parser[S, B] = {
		Parser(input => this.parse(input).orElse(other.parse(input)))
	}

	def map[B](g: A => B): Parser[S, B] = {
		Parser(input => this.parse(input).map { case (v, out) => (g(v), out) })
	}

	def flatMap[B](f: A => Parser[S, B]): Parser[S, B] = this >>= f
	def >>=[B](f: A => Parser[S, B]): Parser[S, B] = {
		Parser(input => {
			for {
				(v, out) <- this.parse(input)
				result <- f(v).parse(out)
			} yield result
		})
	}

	def >>[B](other: => Parser[S, B]): Parser[S, B] = {
		this >>= (_ => other)
	}
	def <<[B](other: => Parser[S, B]): Parser[S, A] = {
		this >>= (x => other >> from(x))
	}

	def satisfy(predicate: A => Boolean): Parser[S, A] = {
		for {
			x <- this
			z <- if (predicate(x)) from[S, A](x) else failure[S, A]
		} yield z
	}

	def filter(predicate: A => Boolean): Parser[S, A] = satisfy(predicate)

	def maybe: Parser[S, Option[A]] = map(Option(_)) <|> from(Option.empty)

	def many: Parser[S, List[A]] = {
		atLeastOnce <|> from(List())
	}

	def takeUntil(predicate: A => Boolean): Parser[S, List[A]] = {
		def not(predicate: A => Boolean): A => Boolean = !predicate(_)
		this.takeWhile(not(predicate))
	}

	def takeWhile(predicate: A => Boolean): Parser[S, List[A]] = {
		this.satisfy(predicate).many
	}

	def atLeastOnce: Parser[S, List[A]] = {
		for {
			v <- this
			vs <- many
		} yield v :: vs
	}

	def separatedBy[Sep](sep: Parser[S, Sep]): Parser[S, List[A]] = {
		this.separatedBy1(sep) <|> from(List())
	}

	def separatedBy1[Sep](sep: Parser[S, Sep]): Parser[S, List[A]] = {
		this >>= (x => (sep >> this).many.map(x::_))
	}
}
object Parser {
	def apply[S, A](parser: S => Option[(A, S)]) = new Parser(parser)

	def from[S, A](a: A): Parser[S, A] = Parser(Some(a, _))

	def failure[S, A]: Parser[S, A] = Parser(_ => None)
}
