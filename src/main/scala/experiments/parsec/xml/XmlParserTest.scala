package experiments.parsec.xml

import experiments.parsec.xml.XmlParser.{ XmlParser, attribute, attributeId, branchNode, xmlToString }

import scala.xml.Utility

object XmlParserTest extends App {

  case class Number(number: String, addition: Option[String] = None)
  sealed abstract class Address(zipCode: String, city: String)
  case class RealAddress(street: String, number: Number, zipCode: String, city: String) extends Address(zipCode: String, city: String)
  case class AntwoordnummerAddress(number: String, zipCode: String, city: String) extends Address(zipCode: String, city: String)
  case class Person(name: String, age: Int, address: Address, mail: Option[String])

  def parseNumber(name: String): XmlParser[Number] = {
    for {
      addition <- attributeId("addition").maybe
      number <- xmlToString(name)
    } yield Number(number, addition)
  }

  def parseRealAddress(name: String): XmlParser[Address] = {
    for {
      // no attributes here
      addr <- branchNode(name) {
        for {
          street <- xmlToString("street")
          number <- parseNumber("number")
          zipCode <- xmlToString("zip-code")
          city <- xmlToString("city")
        } yield RealAddress(street, number, zipCode, city)
      }
    } yield addr
  }

  def parseAntwoordnummerAddress(name: String): XmlParser[Address] = {
    for {
    // no attributes here
      addr <- branchNode(name) {
        for {
          number <- xmlToString("antwoordnummer")
          zipCode <- xmlToString("zip-code")
          city <- xmlToString("city")
        } yield AntwoordnummerAddress(number, zipCode, city)
      }
    } yield addr
  }

  def parseAddress(name: String): XmlParser[Address] = {
    parseRealAddress(name) <|> parseAntwoordnummerAddress(name)
  }

  // root node doesn't have a name
  def parsePerson: XmlParser[Person] = {
    for {
      age <- attribute("age")(_.toInt)
      p <- branchNode("person") {
        for {
          pName <- xmlToString("name")
          address <- parseAddress("address")
          mail <- xmlToString("mail").maybe
        } yield Person(pName, age, address, mail)
      }
    } yield p
  }

  val xml1 = <person age="24">
    <name>Richard van Heest</name>
    <address>
      <street>Prins Bernhardlaan</street>
      <number>116</number>
      <zip-code>3241TA</zip-code>
      <city>Middelharnis</city>
    </address>
    <mail>richard.v.heest@gmail.com</mail>
  </person>

  val xml2 = <person age="24">
    <name>Richard van Heest</name>
    <address>
      <street>Prins Bernhardlaan</street>
      <number addition="a">116</number>
      <zip-code>3241TA</zip-code>
      <city>Middelharnis</city>
    </address>
    <mail>richard.v.heest@gmail.com</mail>
  </person>

  val xml3 = <person age="24">
    <name>Richard van Heest</name>
    <address>
      <antwoordnummer>12345</antwoordnummer>
      <zip-code>3241TA</zip-code>
      <city>Middelharnis</city>
    </address>
  </person>

  println(parsePerson.run(Utility.trim(xml1)))
  println(parsePerson.run(Utility.trim(xml2)))
  println(parsePerson.run(Utility.trim(xml3)))
}
