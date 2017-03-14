package experiments.pickling.xml

import experiments.pickling.xml.XmlPickle.XmlPickle

import scala.xml.{ Node, PrettyPrinter }

object PickleTest extends App {

  case class Number(number: String, addition: Option[String] = None)
  sealed abstract class Address(zipCode: String, city: String)
  case class RealAddress(street: String, number: Number, zipCode: String, city: String) extends Address(zipCode: String, city: String)
  case class AntwoordnummerAddress(number: String, zipCode: String, city: String) extends Address(zipCode: String, city: String)
  case class Person(name: String, age: Int, address: Address, mail: Option[String])

  val obj1 = Person(
    name = "Richard van Heest",
    age = 24,
    address = RealAddress("Prins Bernhardlaan", Number("116"), "3241TA", "Middelharnis"),
    mail = Some("richard.v.heest@gmail.com"))
  val obj2 = obj1.copy(address = RealAddress("Prins Bernhardlaan", Number("116", Option("a")), "3241TA", "Middelharnis"))
  val obj3 = obj1.copy(address = AntwoordnummerAddress("12345", "3241TA", "Middelharnis"), mail = None)

  val street: XmlPickle[String] = XmlPickle.string("street")
  val number: XmlPickle[Number] = XmlPickle.attribute("addition").maybe
    .pair(XmlPickle.string("number"))
    .wrap({ case (a, n) => Number(n, a) })({ case Number(n, a) => (a, n) })
  val antwoordnummer: XmlPickle[String] = XmlPickle.string("antwoordnummer")
  val zipCode: XmlPickle[String] = XmlPickle.string("zipCode")
  val city: XmlPickle[String] = XmlPickle.string("city")

  val address: XmlPickle[Address] = {
    val realAddress: XmlPickle[Address] = street.quad(number, zipCode, city)
      .wrap[Address]({ case (s, n, z, c) => RealAddress(s, n, z, c) })({ case RealAddress(s, n, z, c) => (s, n, z, c) })
    val antwoordnummerAddress: XmlPickle[Address] = antwoordnummer.triple(zipCode, city)
      .wrap[Address]({ case (a, z, c) => AntwoordnummerAddress(a, z, c) })({ case AntwoordnummerAddress(a, z, c) => (a, z, c) })
    val altAddress: XmlPickle[Address] = Pickle.alt[Address, Seq[Node]](Array(realAddress, antwoordnummerAddress)) {
      case _: RealAddress => 0
      case _: AntwoordnummerAddress => 1
    }
    XmlPickle.inside("address")(altAddress)
  }

  val name: XmlPickle[String] = XmlPickle.string("name")
  val age: XmlPickle[Int] = XmlPickle.attribute("age").seq[Int](_.toString)(s => Pickle.lift(s.toInt))
  val prefixedAge: XmlPickle[Int] = XmlPickle.attribute("prefix", "age").seq[Int](_.toString)(s => Pickle.lift(s.toInt))
  val mail: XmlPickle[Option[String]] = XmlPickle.string("mail").maybe

  val person: XmlPickle[Person] = {
    val person: XmlPickle[(String, Address, Option[String])] = XmlPickle.inside("person")(name.triple(address, mail))
    val attrs: XmlPickle[(Int, Int)] = age.pair(prefixedAge)
    attrs.pair(person)
      .wrap({ case ((a, _), (n, ad, m)) => Person(n, a, ad, m) })({ case Person(n, a, ad, m) => ((a, a), (n, ad, m)) })
  }

  val pp = new PrettyPrinter(80, 4)

//  street.pair(number).pickle(("Prins Bernhardlaan", Number("116", Option("a"))), Nil).map(pp.format(_)).foreach(println)
//  street.triple(XmlPickle.attribute("addition").maybe, XmlPickle.string("number")).pickle(("Prins Bernhardlaan", Option("a"), "116"), Nil).map(pp.format(_)).foreach(println) // TODO is this the same as the one above? both in pickle and unpickle
//  street.triple(number, zipCode).pickle(("Prins Bernhardlaan", Number("116", Option("a")), "3241TA"), Nil).map(pp.format(_)).foreach(println)

//  number.pickle(Number("116", Option("a")), Nil).map(pp.format(_)).foreach(println)
//  address.pickle(obj1.address, Nil).map(pp.format(_)).foreach(println)
//  address.pickle(obj2.address, Nil).map(pp.format(_)).foreach(println)

  person.pickle(obj1, Nil).map(pp.format(_)).foreach(println)
}
