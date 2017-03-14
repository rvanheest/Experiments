package experiments.pickling.xml

import experiments.parsec.xml.XmlParser

import scala.xml._

object XmlPickle {

	type XmlPickle[A] = Pickle[A, Seq[Node]]

	def string(name: String): XmlPickle[String] = {
		Pickle(
			pickle = (s: String, xml: Seq[Node]) => <xml>{s}</xml>.copy(label = name) ++ xml,
			unpickle = XmlParser.xmlToString(name))
	}

	def attribute(name: String): XmlPickle[String] = {
		Pickle(
			pickle = (s: String, xml: Seq[Node]) => {
				xml.headOption map {
					case elem: Elem => elem % new UnprefixedAttribute(name, s, Null) ++ xml.tail
					case _ => sys.error("Can only add attributes to elements!")
				} getOrElse sys.error("Cannot add attributes to an empty sequence")
			},
			unpickle = XmlParser.attributeId(name))
	}

	def namespaceAttribute(name: String)(implicit namespace: NamespaceBinding): XmlPickle[String] = {
		Pickle(
			pickle = (s: String, xml: Seq[Node]) => {
				xml.headOption map {
					case elem: Elem => elem % new PrefixedAttribute(namespace.prefix, name, s, Null) ++ xml.tail
					case _ => sys.error("Can only add attributes to elements!")
				} getOrElse sys.error("Cannot add attributes to an empty sequence")
			},
			unpickle = XmlParser.namespaceAttribute(name))
	}

	def inside[A](name: String)(pickleA: XmlPickle[A]): XmlPickle[A] = {
		Pickle(
			pickle = (a: A, xml: Seq[Node]) => <xml>{pickleA.pickle(a, Nil)}</xml>.copy(label = name) ++ xml,
			unpickle = XmlParser.branchNode(name)(pickleA.unpickle))
	}
}
