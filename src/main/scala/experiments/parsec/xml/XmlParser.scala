package experiments.parsec.xml

import experiments.parsec.Parser

import scala.util.{Failure, Success, Try}
import scala.xml.{NamespaceBinding, Node}

object XmlParser {

	type XmlParser[A] = Parser[Seq[Node], A]

	private def nodeItem: XmlParser[Node] = {
		Parser(ns => ns
			.headOption
			.map(head => (Try(head), ns.tail))
			.getOrElse((Failure(new NoSuchElementException("you're trying to parse a node in an empty xml Node")), Seq.empty)))
	}

	def nodeWithName(name: String): XmlParser[Node] = {
		nodeItem.transform {
			case (head, tail) if head.label == name => (Success(head), tail)
			case (head, tail) => (Failure(new NoSuchElementException(s"could not find an element with name '$name'")), head +: tail)
		}
	}

	def xmlToString(name: String): XmlParser[String] = {
		nodeWithName(name).map(_.text)
	}

	def node[T](name: String)(constructor: String => T): XmlParser[T] = {
		xmlToString(name).flatMap(Parser.withException(_)(constructor))
	}

	def branchNode[A](name: String)(subParser: XmlParser[A]): XmlParser[A] = {
		Parser(nodeWithName(name).map(_.child).run(_) match {
			case (Success(childNodes), rest) =>
				val (r, rest2) = subParser.run(childNodes)
				(r, rest2 ++ rest)
			case (Failure(e), rest) => (Failure(e), rest)
		})
	}

	private def attributeItem: XmlParser[Node] = {
		Parser(ns => ns
			.headOption
			.map(head => (Try(head), ns))
			.getOrElse((Failure(new NoSuchElementException("you're trying to parse an attribute in an empty xml Node")), Seq.empty)))
	}

	def attribute[T](attr: String)(constructor: String => T): XmlParser[T] = {
		attributeItem
			.map(_ \@ attr)
			.satisfy(_.nonEmpty)
			.flatMap(Parser.withException(_)(constructor))
	}

	def attributeId(attr: String): XmlParser[String] = {
		attribute(attr)(identity)
	}

	def namespaceAttribute(attrName: String)(implicit namespace: NamespaceBinding): XmlParser[String] = {
		// notice that _.attributes(...) can be null!!!
		attributeItem
			.map(_.attributes(namespace.uri, namespace, attrName))
			.satisfy(xs => xs != null && xs.nonEmpty)
			.map(_.head.text)
	}

	def debugAndFail(pos: String = ""): XmlParser[Nothing] = {
		Parser(xs => sys.error(s"you hit a debug statement at $pos: $xs"))
	}

	def debugAndContinue(pos: String = ""): XmlParser[Unit] = {
		Parser(xs => {
			println(s"you hit a debug statement at $pos: $xs")
			(Success(()), xs)
		})
	}
}
