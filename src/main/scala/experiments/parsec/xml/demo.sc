import experiments.parsec.xml.XmlParser.{XmlParser, _}

import scala.xml.Utility

case class Address(street: String, number: String, zipCode: String, city: String)
case class Person(name: String, age: Int, address: Address)

def address(name: String): XmlParser[Address] = {
	for {
		// attributes
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
def person: XmlParser[Person] = {
	for {
		// attributes
		p <- branchNode("person") {
			for {
				pName <- xmlToString("name")
				age <- xmlToString("age").map(_.toInt)
				address <- address("address")
			} yield Person(pName, age, address)
		}
	} yield p
}

// this should be failing: zipCode vs zip-code
val xml = <person>
	<name>Richard van Heest</name>
	<age>24</age>
	<address>
		<street>Prins Bernhardlaan</street>
		<number>116</number>
		<zipCode>3241TA</zipCode>
		<city>Middelharnis</city>
	</address>
</person>

person.run(Utility.trim(xml))
