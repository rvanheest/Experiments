import experiments.parsec.xml.XmlParser.{ XmlParser, _ }

import scala.xml._

case class Address(street: String, number: String, zipCode: String, city: String)
case class Person(name: String, age: Int, address: Address, mail: String)

def parseAddress(name: String): XmlParser[Address] = {
	for {
		// no attributes here
		addr <- branchNode(name) {
			for {
				street <- xmlToString("street")
				number <- xmlToString("number")
				zipCode <- xmlToString("zip-code")
				city <- xmlToString("city")
			} yield Address(street, number, zipCode, city)
		}
	} yield addr
}

// root node doesn't have a name
def parsePerson: XmlParser[Person] = {
	for {
		age <- attribute("age")(_.toInt)
		p <- branchNode("person") {
			for {
				pName <- xmlToString("name")
				address <- parseAddress("address")
				mail <- xmlToString("mail")
			} yield Person(pName, age, address, mail)
		}
	} yield p
}

val xml = <person age="24">
	<name>Richard van Heest</name>
	<address>
		<street>Prins Bernhardlaan</street>
		<number>116</number>
		<zip-code>3241TA</zip-code>
		<city>Middelharnis</city>
	</address>
	<mail>richard.v.heest@gmail.com</mail>
</person>

parsePerson.run(Utility.trim(xml))

////////////
// to xml //
////////////
trait Pickle[+T] {
	def pickle: T
	def map[S](f: T => S): Pickle[S] = Pickle.lift(f(this.pickle))
}
object Pickle {
	def lift[T](t: => T) = new Pickle[T] {
		def pickle: T = t
	}
	def combine[T](inner: Pickle[T]*): Pickle[Seq[T]] = new Pickle[Seq[T]] {
		def pickle = inner.map(_.pickle)
	}
}

implicit class MetadataPickle(self: Pickle[MetaData]) {
	def ++(other: Pickle[MetaData]): Pickle[MetaData] = new Pickle[MetaData] {
		def pickle = MetaData.concatenate(self.pickle, other.pickle)
	}
}

def string(value: String): Pickle[Node] = {
	Pickle.lift(new Text(value))
}
def attr(name: String)(value: String): Pickle[MetaData] = {
	Pickle.lift(new UnprefixedAttribute(name, value, Null))
}
def attr(prefix: String, name: String)(value: String): Pickle[MetaData] = {
	Pickle.lift(new PrefixedAttribute(prefix, name, value, Null))
}
def emptyAttr: Pickle[MetaData] = {
	Pickle.lift(Null)
}
def nodeWithName(name: String)(inner: Pickle[_ >: Node])(attributes: Pickle[MetaData] = emptyAttr): Pickle[Node] = {
	Pickle.lift(<key>{inner.pickle}</key>.copy(label = name, attributes = attributes.pickle))
}

val street = nodeWithName("street")(string("Prins Bernhardlaan"))()
val number = nodeWithName("number")(string("116"))()
val zipcode = nodeWithName("zip-code")(string("3241TA"))()
val city = nodeWithName("city")(string("Middelharnis"))()
val address = nodeWithName("address")(Pickle.combine(street, number, zipcode, city))()
val name = nodeWithName("name")(string("Richard van Heest"))()
val mail = nodeWithName("mail")(string("richard.v.heest@gmail.com"))()
val age = attr("age")("24")
val foo = attr("prefix", "age")("24")
val person = nodeWithName("person")(Pickle.combine(name, address, mail))(age ++ foo)

new PrettyPrinter(80, 4).format(person.pickle)
