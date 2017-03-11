import experiments.pickling.xml.XmlPickle.XmlPickle
import experiments.pickling.xml.{Pickle, XmlPickle}

import scala.xml._

sealed abstract class Address(zipCode: String, city: String)
case class RealAddress(street: String, number: String, zipCode: String, city: String) extends Address(zipCode: String, city: String)
case class AntwoordnummerAddress(number: String, zipCode: String, city: String) extends Address(zipCode: String, city: String)
case class Person(name: String, age: Int, address: Address, mail: Option[String])

val obj1 = Person(
	name = "Richard van Heest",
	age = 24,
	address = RealAddress("Prins Bernhardlaan", "116", "3241TA", "Middelharnis"),
	mail = Some("richard.v.heest@gmail.com"))
val obj2 = obj1.copy(address = AntwoordnummerAddress("12345", "3241TA", "Middelharnis"), mail = None)

val street: XmlPickle[String] = XmlPickle.string("street")
val number: XmlPickle[String] = XmlPickle.string("number")
val antwoordnummer: XmlPickle[String] = XmlPickle.string("antwoordnummer")
val zipCode: XmlPickle[String] = XmlPickle.string("zipCode")
val city: XmlPickle[String] = XmlPickle.string("city")

val address: XmlPickle[Address] = {
	val realAddress: XmlPickle[Address] = street.quad(number, zipCode, city)
		.wrap[Address]({ case (s, n, z, c) => RealAddress(s, n, z, c) })({ case RealAddress(s, n, z, c) => (s, n, z, c) })
	val antwoordnummerAddress: XmlPickle[Address] = antwoordnummer.triple(zipCode, city)
		.wrap[Address]({ case (a, z, c) => AntwoordnummerAddress(a, z, c) })({ case AntwoordnummerAddress(a, z, c) => (a, z, c) })
	val altAddress: XmlPickle[Address] = Pickle.alt[Address, Node](Array(realAddress, antwoordnummerAddress)) {
		case _: RealAddress => 0
		case _: AntwoordnummerAddress => 1
	}
	XmlPickle.inside("address")(altAddress)
}

val name: XmlPickle[String] = XmlPickle.string("name")
val age: XmlPickle[Int] = XmlPickle.attribute("age").seq[Int](_.toString)(s => Pickle.lift(s.toInt))
val prefixedAge: XmlPickle[Int] = XmlPickle.attribute("prefix", "age").seq[Int](_.toString)(s => Pickle.lift(s.toInt))
val mail: XmlPickle[Option[String]] = XmlPickle.string("mail").maybe
val person: XmlPickle[Person] = name.quad(age.pair(prefixedAge), address, mail)
	.wrap(x => Person(x._1, x._2._1, x._3, x._4))({ case Person(n, a, ad, m) => (n, (a, a), ad, m) })

val pp = new PrettyPrinter(80, 4)

val res1 = person.pickle(obj1, <person/>)
pp.format(res1)
//val res2 = person.pickle(obj2, <person/>)
//pp.format(res2)
