package experiments.pickling.xml

import scala.language.postfixOps
import scala.xml.{ NamespaceBinding, PrettyPrinter, TopScope }

object XmlPickleTest extends App {

  case class Number(number: String, addition: Option[String] = None)
  sealed abstract class Address(zipCode: String, city: String)
  case class RealAddress(street: String, number: Number, zipCode: String, city: String) extends Address(zipCode, city)
  case class AntwoordnummerAddress(number: String, zipCode: String, city: String) extends Address(zipCode, city)
  case class Person(name: String, age: Int, address: Address, mail: Option[String])

  val obj1 = Person(
    name = "Richard van Heest",
    age = 24,
    address = RealAddress("Prins Bernhardlaan", Number("116"), "3241TA", "Middelharnis"),
    mail = Some("richard.v.heest@gmail.com"))
  val obj2 = obj1.copy(address = RealAddress("Prins Bernhardlaan", Number("116", Option("a")), "3241TA", "Middelharnis"))
  val obj3 = obj1.copy(address = AntwoordnummerAddress("12345", "3241TA", "Middelharnis"), mail = None, name = "foo")

  def pickleNumber(name: String): XmlPickle[Number] = {
    for {
      addition <- XmlPickle.attribute("addition").maybe.seq[Number](_.addition)
      number <- XmlPickle.string(name).seq[Number](_.number)
    } yield Number(number, addition)
  }

  def pickleRealAddress(name: String): XmlPickle[RealAddress] = {
    XmlPickle.branchNode(name) {
      for {
        s <- XmlPickle.string("street").seq[RealAddress](_.street)
        n <- pickleNumber("number").seq[RealAddress](_.number)
        z <- XmlPickle.string("zipCode").seq[RealAddress](_.zipCode)
        c <- XmlPickle.string("city").seq[RealAddress](_.city)
      } yield RealAddress(s, n, z, c)
    }
  }

  def pickleAntwoordnummerAddress(name: String): XmlPickle[AntwoordnummerAddress] = {
    XmlPickle.branchNode(name) {
      for {
        a <- XmlPickle.string("antwoordnummer").seq[AntwoordnummerAddress](_.number)
        z <- XmlPickle.string("zipCode").seq[AntwoordnummerAddress](_.zipCode)
        c <- XmlPickle.string("city").seq[AntwoordnummerAddress](_.city)
      } yield AntwoordnummerAddress(a, z, c)
    }
  }

  def pickleAddress(name: String): XmlPickle[Address] = {
    pickleRealAddress(name).upcast[Address] orElse pickleAntwoordnummerAddress(name).upcast[Address]
  }

  // root node doesn't have a name
  def picklePerson: XmlPickle[Person] = {
    implicit val xlinkNamespace = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    for {
      a <- XmlPickle.attribute("age").toInt.seq[Person](_.age)
      _ <- XmlPickle.namespaceAttribute("age").toInt.seq[Person](_.age)
      p <- XmlPickle.branchNode("person") {
        for {
          n <- XmlPickle.string("name").seq[Person](_.name)
          addr <- pickleAddress("address").seq[Person](_.address)
          m <- XmlPickle.string("mail").maybe.seq[Person](_.mail)
        } yield Person(n, a, addr, m)
      }.seq
    } yield p
  }

  val pp = new PrettyPrinter(80, 4)

  val personXml1 = picklePerson.pickle(obj1, Nil)
  for (xml <- personXml1;
       node <- xml) {
    println(pp.format(node))
  }
  val (person1, rest1) = picklePerson.unpickle(personXml1.get)
  println(person1)
  println(rest1.isEmpty)
  for (p <- person1) {
    println(obj1 == p)
  }
  println

  val personXml2 = picklePerson.pickle(obj2, Nil)
  for (xml <- personXml2;
       node <- xml) {
    println(pp.format(node))
  }
  val (person2, rest2) = picklePerson.unpickle(personXml2.get)
  println(person2)
  println(rest2.isEmpty)
  for (p <- person2) {
    println(obj2 == p)
  }
  println

  val personXml3 = picklePerson.pickle(obj3, Nil)
  for (xml <- personXml3;
       node <- xml) {
    println(pp.format(node))
  }
  val (person3, rest3) = picklePerson.unpickle(personXml3.get)
  println(person3)
  println(rest3.isEmpty)
  for (p <- person3) {
    println(obj3 == p)
  }
}
