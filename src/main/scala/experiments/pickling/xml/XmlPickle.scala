package experiments.pickling.xml

import scala.xml._

object XmlPickle {

	type XmlPickle[T] = Pickle[T, Seq[Node]]

	def string(name: String): XmlPickle[String] = new Pickle[String, Seq[Node]] {
		override def pickle(s: String, xml: Seq[Node]): Seq[Node] = {
			<xml>{s}</xml>.copy(label = name) ++ xml
		}
	}

	def attribute(name: String): XmlPickle[String] = new Pickle[String, Seq[Node]] {
		override def pickle(s: String, xml: Seq[Node]): Seq[Node] = {
			xml.headOption map {
				case elem: Elem => elem % new UnprefixedAttribute(name, s, Null) ++ xml.tail
				case _ => sys.error("Can only add attributes to elements!")
			} getOrElse sys.error("Cannot add attributes to an empty sequence")
		}
	}

	def attribute(prefix: String, name: String): XmlPickle[String] = new Pickle[String, Seq[Node]] {
		override def pickle(s: String, xml: Seq[Node]): Seq[Node] = {
			xml.headOption map {
				case elem: Elem => elem % new PrefixedAttribute(prefix, name, s, Null) ++ xml.tail
				case _ => sys.error("Can only add attributes to elements!")
			} getOrElse sys.error("Cannot add attributes to an empty sequence")
		}
	}

	def inside[A](name: String)(pickleA: XmlPickle[A]): XmlPickle[A] = new Pickle[A, Seq[Node]] {
		override def pickle(a: A, xml: Seq[Node]): Seq[Node] = {
			<xml>{pickleA.pickle(a, Nil)}</xml>.copy(label = name) ++ xml
		}
	}
}
