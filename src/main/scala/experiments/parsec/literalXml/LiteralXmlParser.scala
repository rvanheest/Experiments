package experiments.parsec.literalXml

import experiments.parsec.Parser
import experiments.parsec.Parser._
import experiments.parsec.string.StringParser._

import scala.util.Success

object LiteralXmlParser {
	type AttrName = String
	type AttrVal = String

	case class Attribute(name: AttrName, value: AttrVal)

	trait XML
	case class Element(name: String, attrs: List[Attribute], childs: List[XML]) extends XML
	case class SelfClosingTag(name: String, attrs: List[Attribute]) extends XML
	case class Decl(attrs: List[Attribute]) extends XML
	case class Body(body: String) extends XML

	def document: StringParser[List[XML]] = {
		for {
			_ <- spaces
			y <- xmlDecl <|> tag
			_ <- spaces
			x <- tag.many
			_ <- spaces
		} yield y :: x
	}

	def xmlDecl: StringParser[XML] = {
		for {
			_ <- string("<?xml")
			_ <- spaces
			attr <- attribute.many
			_ <- spaces
			_ <- string("?>")
		} yield Decl(attr)
	}

	def debug: StringParser[String] = Parser(xs => {
		println(s"debug = $xs")
		Success(("", xs))
	})

	def tag: StringParser[XML] = {
		for {
			_ <- char('<')
			_ <- spaces
			name <- (letter <|> digit).many.map(_.mkString)
			_ <- spaces
			attr <- attribute.many
			_ <- spaces
			close <- string("/>") <|> string(">")
			result <- {
				if (close.length == 2) from[String, XML](SelfClosingTag(name, attr))
				else for {
					elementBody <- elementBody.many
					_ <- endTag(name)
					_ <- spaces
				} yield Element(name, attr, elementBody)
			}
		} yield result
	}

	def elementBody: StringParser[XML] = spaces >> tag <|> text

	def endTag(str: String): StringParser[String] = string("</") >> string(str) << char('>')

	def text: StringParser[XML] = item.noneOf("><".toList).atLeastOnce.map(cs => Body(cs.mkString))

	def attribute: StringParser[Attribute] = {
		for {
			name <- item.noneOf("= />".toList).many.map(_.mkString)
			_ <- spaces
			_ <- char('=')
			_ <- spaces
			_ <- char('"')
			value <- item.noneOf("\"".toList).many.map(_.mkString)
			_ <- char('"')
			_ <- spaces
		} yield Attribute(name, value)
	}
}
