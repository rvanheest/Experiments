package experiments.parsec.string

import experiments.parsec.Parser
import experiments.parsec.Parser.from

import scala.language.postfixOps
import scala.util.{Failure, Success}

object StringParser {

	type StringParser[A] = Parser[String, A]

	def item: StringParser[Char] = {
		Parser(_.toList match {
			case x :: xs => Success((x, xs.mkString))
			case Nil => Failure(new NoSuchElementException("you're trying to parse a character in an empty String"))
		})
	}

	def digit: StringParser[Char] = item.satisfy(_.isDigit)

	def number: StringParser[String] = digit.atLeastOnce.map(_.mkString)

	def lower: StringParser[Char] = item.satisfy(_.isLower)

	def upper: StringParser[Char] = item.satisfy(_.isUpper)

	def letter: StringParser[Char] = item.satisfy(_.isLetter)

	def alphanum: StringParser[Char] = item.satisfy(c => c.isLetter || c.isDigit)

	def char(c: Char): StringParser[Char] = item.satisfy(c ==)

	def space: StringParser[Char] = char(' ')

	def spaces: StringParser[Unit] = space skipMany

	def string(s: String): StringParser[String] = s.toList match {
		case x :: xs => for {
			_ <- char(x)
			_ <- string(xs.mkString)
		} yield s
		case Nil => from("")
	}
}
