package experiments.parsec

import experiments.parsec.Parser.from

import scala.language.postfixOps

object StringParser {

	type StringParser[A] = Parser[String, A]

	def item: StringParser[Char] = {
		Parser(_.toList match {
			case x :: xs => Some((x, xs.mkString))
			case Nil => None
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
