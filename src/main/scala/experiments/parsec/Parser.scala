package experiments.parsec

import scala.util.{Failure, Success, Try}

class Parser[S, A](parse: S => Try[(A, S)]) {

	def run(s: S): Try[(A, S)] = parse(s)

	def eval(s: S): Try[A] = parse(s).map(_._1)

	def execute(s: S): Try[S] = parse(s).map(_._2)

	def orElse[B >: A](other: => Parser[S, B]): Parser[S, B] = {
		Parser(st => parse(st).orElse(other.run(st)))
	}
	def <|>[B >: A](other: => Parser[S, B]): Parser[S, B] = {
		this.orElse(other)
	}

	def map[B](f: A => B): Parser[S, B] = {
		Parser(st => parse(st).map { case (a, s) => (f(a), s) })
	}

	def doOnNext[Ignore](f: A => Ignore): Parser[S, A] = {
		map(a => { f(a); a })
	}

	def as[B](b: => B): Parser[S, B] = {
		map(_ => b)
	}

	def flatMap[B](f: A => Parser[S, B]): Parser[S, B] = {
		Parser(st => parse(st).flatMap { case (a, s) => f(a).run(s) })
	}
	def >>=[B](f: A => Parser[S, B]): Parser[S, B] = {
		this.flatMap(f)
	}

	def >>[B](other: => Parser[S, B]): Parser[S, B] = {
		this >>= (_ => other)
	}

	def <<[B](other: => Parser[S, B]): Parser[S, A] = {
		this >>= (x => other >> Parser.from(x))
	}

	def filter(predicate: A => Boolean): Parser[S, A] = satisfy(predicate)
	def satisfy(predicate: A => Boolean): Parser[S, A] = {
		this >>= (x => if (predicate(x)) Parser.from(x) else Parser.empty)
	}

	def noneOf(as: List[A]): Parser[S, A] = {
		satisfy(!as.contains(_))
	}

	def maybe: Parser[S, Option[A]] = {
		map(Option(_)) <|> Parser.from(Option.empty)
	}

	def many: Parser[S, List[A]] = {
		atLeastOnce <|> Parser.from(Nil)
	}

	def atLeastOnce: Parser[S, List[A]] = {
		for {
			x <- this
			xs <- many
		} yield x :: xs
	}

	def takeUntil(predicate: A => Boolean): Parser[S, List[A]] = {
		takeWhile(!predicate(_))
	}

	def takeWhile(predicate: A => Boolean): Parser[S, List[A]] = {
		satisfy(predicate).many
	}

	def separatedBy[Sep](sep: Parser[S, Sep]): Parser[S, List[A]] = {
		separatedBy1(sep) <|> Parser.from(Nil)
	}

	def separatedBy1[Sep](sep: Parser[S, Sep]): Parser[S, List[A]] = {
		for {
			x <- this
			xs <- (sep >> this).many
		} yield x :: xs
	}

	def skipMany: Parser[S, Unit] = this >> skipMany <|> Parser.from(())
}
object Parser {
	def apply[S, A](parser: S => Try[(A, S)]) = new Parser(parser)

	def from[S, A](a: A): Parser[S, A] = Parser(Success(a, _))

	def empty[S, A]: Parser[S, A] = Parser(_ => Failure(new NoSuchElementException("empty parser")))

	def failure[S, A](e: Throwable): Parser[S, A] = Parser(_ => Failure(e))
}
