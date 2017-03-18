package experiments.pickling.xml

import experiments.parsec.xml.XmlParser
import experiments.pickling.{ Pickle, PickleBuilder }

import scala.language.reflectiveCalls
import scala.util.Try
import scala.xml._

case class XmlPickle[A](override val pickle: (A, Seq[Node]) => Try[Seq[Node]],
												override val unpickle: Seq[Node] => (Try[A], Seq[Node]))
	extends Pickle[A, Seq[Node]](pickle, unpickle) {

	type Repr[X] = XmlPickle[X]

	protected[this] implicit def builder[X]: PickleBuilder[X, Seq[Node], XmlPickle[X]] = XmlPickle.xmlPickleBuilder

	def toInt(implicit ev: A =:= String, ev2: String =:= A): Repr[Int] = {
		this.seq[Int](_.toString).map(ev(_).toInt)
	}
}

object XmlPickle {

	implicit def xmlPickleBuilder[X]: PickleBuilder[X, Seq[Node], XmlPickle[X]] = {
		new PickleBuilder[X, Seq[Node], XmlPickle[X]] {
			def apply(pickle: (X, Seq[Node]) => Try[Seq[Node]], unpickle: Seq[Node] => (Try[X], Seq[Node])): XmlPickle[X] = {
				XmlPickle(pickle, unpickle)
			}
		}
	}

	def string(name: String): XmlPickle[String] = {
		XmlPickle(
			pickle = (s: String, xml: Seq[Node]) => Try { <xml>{s}</xml>.copy(label = name) ++ xml },
			unpickle = XmlParser.xmlToString(name).run)
	}

	def node[A](name: String)(constructor: String => A)(destructor: A => String): XmlPickle[A] = {
		XmlPickle(
			pickle = (a: A, xml: Seq[Node]) => Try { <xml>{destructor(a)}</xml>.copy(label = name) ++ xml },
			unpickle = XmlParser.node(name)(constructor).run)
	}

	def attribute(name: String): XmlPickle[String] = {
		XmlPickle(
			pickle = (s: String, xml: Seq[Node]) => Try {
				xml.headOption map {
					case elem: Elem => elem % new UnprefixedAttribute(name, s, Null) ++ xml.tail
					case _ => sys.error("Can only add attributes to elements!")
				} getOrElse sys.error("Cannot add attributes to an empty sequence")
			},
			unpickle = XmlParser.attributeId(name).run)
	}

	def namespaceAttribute(name: String)(implicit namespace: NamespaceBinding): XmlPickle[String] = {
		XmlPickle(
			pickle = (s: String, xml: Seq[Node]) => Try {
				xml.headOption map {
					case elem: Elem => elem % new PrefixedAttribute(namespace.prefix, name, s, Null) ++ xml.tail
					case _ => sys.error("Can only add attributes to elements!")
				} getOrElse sys.error("Cannot add attributes to an empty sequence")
			},
			unpickle = XmlParser.namespaceAttribute(name).run)
	}

	def inside[A](name: String)(pickleA: XmlPickle[A]): XmlPickle[A] = {
		import Pickle.unpickleAsParser
		XmlPickle(
			pickle = (a: A, xml: Seq[Node]) => pickleA.pickle(a, Nil).map(nodes => <xml>{nodes}</xml>.copy(label = name) ++ xml),
			unpickle = XmlParser.branchNode(name)(pickleA.unpickle).run)
	}
}
