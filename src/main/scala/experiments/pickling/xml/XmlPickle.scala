package experiments.pickling.xml

import scala.xml._
import scala.xml.transform.RewriteRule

object XmlPickle {

	type XmlPickle[T] = Pickle[T, Seq[Node]]

	private class AddChildrenTo(newChild: Seq[Node]) extends RewriteRule {
		override def transform(n: Node): Node = n match {
			case Elem(prefix, lbl, attribs, scope, child @ _*) =>
				Elem(prefix, lbl, attribs, scope, false, newChild ++ child : _*)
			case _ => sys.error("Can only add children to elements!")
		}
	}

	def string(name: String): XmlPickle[String] = new Pickle[String, Seq[Node]] {
		override def pickle(s: String, xml: Seq[Node]): Seq[Node] = {
			new AddChildrenTo(<xml>{s}</xml>.copy(label = name)).transform(xml)
		}
	}

	def attribute(name: String): XmlPickle[String] = new Pickle[String, Seq[Node]] {
		override def pickle(s: String, xml: Seq[Node]): Seq[Node] = xml map {
			case elem: Elem => elem % new UnprefixedAttribute(name, s, Null)
			case _ => sys.error("Can only add children to elements!")
		}
	}

	def attribute(prefix: String, name: String): XmlPickle[String] = new Pickle[String, Seq[Node]] {
		override def pickle(s: String, xml: Seq[Node]): Seq[Node] = xml map {
			case elem: Elem => elem % new PrefixedAttribute(prefix, name, s, Null)
			case _ => sys.error("Can only add children to elements!")
		}
	}

	def inside[A](name: String)(pickleA: XmlPickle[A]): XmlPickle[A] = new Pickle[A, Seq[Node]] {
		override def pickle(a: A, xml: Seq[Node]): Seq[Node] = {
			new AddChildrenTo(pickleA.pickle(a, <xml/>.copy(label = name))).transform(xml)
		}
	}
}
