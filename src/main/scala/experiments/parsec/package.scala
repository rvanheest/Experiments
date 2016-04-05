package experiments

import experiments.parsec.Parser.{failure, from}
import experiments.parsec.StringParser.StringParser

package object parsec {

	implicit class StringParserX[A](val parser: StringParser[A]) extends AnyVal {
		def noneOf(s: String): StringParser[A] = {
			parser.satisfy(!s.contains(_))
		}
	}

	implicit class ABX[S, A, B](val f: A => Parser[S, B]) extends AnyVal {
		def =<<(parser: Parser[S, A]) = parser >>= f
	}

	implicit class FuncParser[S, A, B](val parser: Parser[S, A => B]) extends AnyVal {
		def <*>(parser2: Parser[S, A]): Parser[S, B] = {
			Parser(input => for {
				(aToB, out) <- parser parse input
				(v, out2) <- parser2 parse out
			} yield (aToB(v), out2))
		}
	}

	implicit class ParserParser[S, A](val parser: Parser[S, Parser[S, A]]) extends AnyVal {
		def flatten: Parser[S, A] = parser >>= identity
	}

	implicit class ParserX[S, A](val parser: Parser[S, A]) extends AnyVal {
		def map[B](g: A => B): Parser[S, B] = {
			Parser(input => parser parse input map { case (v, out) => (g(v), out) })
		}

		def replace[B](b: B): Parser[S, B] = {
			parser map (_ => b)
		}

		def *>[B](other: Parser[S, B]): Parser[S, B] = {
			val id = (b: B) => b

			(parser replace id) <*> other
		}

		def <*[B](other: Parser[S, B]): Parser[S, A] = {
			liftA2(other)(a => _ => a)
		}

		def <**>[B](other: Parser[S, A => B]): Parser[S, B] = {
			liftA2(other)(a => _(a))
		}

		def liftA[B](f: A => B): Parser[S, B] = {
			from[S, A => B](f) <*> parser
		}

		def liftA2[B, C](other: Parser[S, B])(f: A => B => C): Parser[S, C] = {
			(parser map f) <*> other
		}

		def liftA3[B, C, D](otherB: Parser[S, B], otherC: Parser[S, C])(f: A => B => C => D): Parser[S, D] = {
			(parser map f) <*> otherB <*> otherC
		}

		def flatMap[B](f: A => Parser[S, B]): Parser[S, B] = >>= (f)
		def >>=[B](f: A => Parser[S, B]): Parser[S, B] = {
			Parser(input => for {
				(v, out) <- parser parse input
				result <- f(v) parse out
			} yield result)
		}

		def >>[B](other: => Parser[S, B]): Parser[S, B] = {
			>>= (_ => other)
		}

		def <<[B](other: => Parser[S, B]): Parser[S, A] = {
			>>= (x => other >>= (_ => from(x)))
		}

		def orElse[B >: A](other: => Parser[S, B]): Parser[S, B] = <|> (other)
		def mplus[B >: A](other: => Parser[S, B]): Parser[S, B] = <|> (other)
		def <|>[B >: A](other: => Parser[S, B]): Parser[S, B] = {
			Parser(input => parser parse input orElse other.parse(input))
		}

		def filter(predicate: A => Boolean): Parser[S, A] = parser satisfy predicate
		def satisfy(predicate: A => Boolean): Parser[S, A] = {
			>>= (x => if (predicate(x)) from(x) else failure)
		}

		def noneOf(as: List[A]): Parser[S, A] = {
			satisfy (!as.contains(_))
		}

		def maybe: Parser[S, Option[A]] = map(Option(_)) <|> from(Option.empty)

		def many: Parser[S, List[A]] = atLeastOnce <|> from(Nil)

		def atLeastOnce: Parser[S, List[A]] = {
			>>= (v => many map (v :: _))
		}

		def takeUntil(predicate: A => Boolean): Parser[S, List[A]] = {
			takeWhile(!predicate(_))
		}

		def takeWhile(predicate: A => Boolean): Parser[S, List[A]] = {
			satisfy(predicate).many
		}

		def separatedBy[Sep](sep: Parser[S, Sep]): Parser[S, List[A]] = {
			separatedBy1(sep) <|> from(Nil)
		}

		def separatedBy1[Sep](sep: Parser[S, Sep]): Parser[S, List[A]] = {
			>>= (x => (sep >> parser).many.map(x :: _))
		}

		def skipMany: Parser[S, Unit] = parser >> parser.skipMany <|> from(())
	}
}
