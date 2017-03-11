package experiments.pickling.xml

import scala.xml._
import scala.xml.transform.RewriteRule

object XmlPickle {

	type XmlPickle[T] = Pickle[T, Node]

	private class AddChildrenTo(newChild: Node) extends RewriteRule {
		override def transform(n: Node): Node = n match {
			case Elem(prefix, lbl, attribs, scope, child @ _*) =>
				Elem(prefix, lbl, attribs, scope, false, newChild ++ child : _*)
			case _ => sys.error("Can only add children to elements!")
		}
	}

	def string(name: String): XmlPickle[String] = new Pickle[String, Node] {
		override def pickle(s: String, xml: Node): Node = {
			new AddChildrenTo(<xml>{s}</xml>.copy(label = name)).transform(xml)
		}
	}

	def attribute(name: String): XmlPickle[String] = new Pickle[String, Node] {
		override def pickle(s: String, xml: Node): Node = xml match {
			case elem: Elem => elem % new UnprefixedAttribute(name, s, Null)
			case _ => sys.error("Can only add children to elements!")
		}
	}

	def attribute(prefix: String, name: String): XmlPickle[String] = new Pickle[String, Node] {
		override def pickle(s: String, xml: Node): Node = xml match {
			case elem: Elem => elem % new PrefixedAttribute(prefix, name, s, Null)
			case _ => sys.error("Can only add children to elements!")
		}
	}

	def inside[A](name: String)(pickleA: XmlPickle[A]): XmlPickle[A] = new Pickle[A, Node] {
		override def pickle(a: A, xml: Node): Node = {
			new AddChildrenTo(pickleA.pickle(a, <xml/>.copy(label = name))).transform(xml)
		}
	}
}
