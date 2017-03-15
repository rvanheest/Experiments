package experiments.pickling.xml

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
  val obj3 = obj1.copy(address = AntwoordnummerAddress("12345", "3241TA", "Middelharnis"), mail = None)

  val street = XmlPickle.string("street")
  val number = XmlPickle.attribute("addition").maybe.pair(XmlPickle.string("number"))
    .wrap { case (a, n) => Number(n, a) }
    .unwrap { case Number(n, a) => (a, n) }
  val antwoordnummer = XmlPickle.string("antwoordnummer")
  val zipCode = XmlPickle.string("zipCode")
  val city = XmlPickle.string("city")

  val address = {
    val realAddress = street.quad(number, zipCode, city)
      .wrap[Address] { case (s, n, z, c) => RealAddress(s, n, z, c) }
      .unwrap { case RealAddress(s, n, z, c) => (s, n, z, c) }
    val antwoordnummerAddress = antwoordnummer.triple(zipCode, city)
      .wrap[Address] { case (a, z, c) => AntwoordnummerAddress(a, z, c) }
      .unwrap { case AntwoordnummerAddress(a, z, c) => (a, z, c) }
    XmlPickle.inside("address")(realAddress orElse antwoordnummerAddress)
  }

  val name = XmlPickle.string("name")
//  val age = XmlPickle.attribute("age").seq[Int](_.toString).map(_.toInt)
  val age = XmlPickle.attribute("age").toInt
  implicit val xlinkNamespace = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
//  val prefixedAge = XmlPickle.namespaceAttribute("age")(xlinkNamespace).seq[Int](_.toString).map(_.toInt)
  val prefixedAge = XmlPickle.namespaceAttribute("age").toInt
  val mail = XmlPickle.string("mail").maybe

  val person = {
    val person: XmlPickle[(String, Address, Option[String])] = XmlPickle.inside("person")(name.triple(address, mail))
    val attrs: XmlPickle[(Int, Int)] = age.pair(prefixedAge)
    attrs.pair(person)
      .wrap { case ((a, _), (n, ad, m)) => Person(n, a, ad, m) }
      .unwrap { case Person(n, a, ad, m) => ((a, a), (n, ad, m)) }
  }

  val pp = new PrettyPrinter(80, 4)

  val personXml1 = person.pickle(obj1, Nil)
  personXml1.map(pp.format(_)).foreach(println)
  val personRes1 = person.unpickle.run(personXml1)
  println(personRes1)
  println(personRes1._1.get == obj1)
  println

  val personXml2 = person.pickle(obj2, Nil)
  personXml2.map(pp.format(_)).foreach(println)
  val personRes2 = person.unpickle.run(personXml2)
  println(personRes2)
  println(personRes2._1.get == obj2)
  println

  val personXml3 = person.pickle(obj3, Nil)
  personXml3.map(pp.format(_)).foreach(println)
  val personRes3 = person.unpickle.run(personXml3)
  println(personRes3)
  println(personRes3._1.get == obj3)
}
