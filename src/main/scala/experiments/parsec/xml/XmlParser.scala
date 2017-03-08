package experiments.parsec.xml

import experiments.parsec.Parser

import scala.util.{Failure, Success}
import scala.xml.{NamespaceBinding, Node}

object XmlParser {

	type XmlParser[A] = Parser[Seq[Node], A]

	private def nodeItem: XmlParser[Node] = {
		Parser(ns => ns
			.headOption
			.map(head => Success(head, ns.tail))
			.getOrElse(Failure(new NoSuchElementException("you're trying to parse a node in an empty xml Node"))))
	}

	private def withException[T](s: String)(constructor: String => T): XmlParser[T] = {
		try { Parser.from(constructor(s)) }
		catch { case e: Throwable => Parser.failure(e) }
	}

	def nodeWithName(name: String): XmlParser[Node] = {
		nodeItem.satisfy(_.label == name)
	}

	def xmlToString(name: String): XmlParser[String] = {
		nodeWithName(name).map(_.text)
	}

	def node[T](name: String)(constructor: String => T): XmlParser[T] = {
		xmlToString(name)
			.flatMap(withException(_)(constructor))
	}

	def branchNode[A](name: String)(subParser: XmlParser[A]): XmlParser[A] = {
		Parser(input => {
			nodeWithName(name).map(_.child).run(input).flatMap {
				case (childNodes, rest) =>
					subParser.run(childNodes).map {
						case (result, rest2) => (result, rest2 ++ rest)
					}
			}
		})
	}

	private def attributeItem: XmlParser[Node] = {
		Parser(ns => ns
			.headOption
			.map(head => Success(head, ns))
			.getOrElse(Failure(new NoSuchElementException("you're trying to parse an attribute in an empty xml Node"))))
	}

	def attribute[T](attr: String)(constructor: String => T): XmlParser[T] = {
		attributeItem
			.map(_ \@ attr)
			.satisfy(_.nonEmpty)
			.flatMap(withException(_)(constructor))
	}

	def attributeId(attr: String): XmlParser[String] = {
		attribute(attr)(identity)
	}

	def namespaceAttribute(attrName: String)(implicit nsURL: String, namespace: NamespaceBinding): XmlParser[String] = {
		// notice that _.attributes(...) can be null!!!
		attributeItem
			.map(_.attributes(nsURL, namespace, attrName))
			.satisfy(xs => xs != null && xs.nonEmpty)
			.map(_.head.text)
	}

	def debugAndFail(pos: String = ""): XmlParser[Nothing] = {
		Parser(xs => sys.error(s"you hit a debug statement at $pos: $xs"))
	}

	def debugAndContinue(pos: String = ""): XmlParser[Unit] = {
		Parser(xs => {
			println(s"you hit a debug statement at $pos: $xs")
			Success((), xs)
		})
	}
}
