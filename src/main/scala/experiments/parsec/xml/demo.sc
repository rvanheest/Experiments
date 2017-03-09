import experiments.parsec.xml.XmlParser.{XmlParser, _}

import scala.xml.Utility

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
