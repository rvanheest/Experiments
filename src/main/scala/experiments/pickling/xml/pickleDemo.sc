import experiments.pickling.xml.Pickle

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

val street: Pickle[String] = Pickle.string("street")
val number: Pickle[String] = Pickle.string("number")
val antwoordnummer: Pickle[String] = Pickle.string("antwoordnummer")
val zipCode: Pickle[String] = Pickle.string("zipCode")
val city: Pickle[String] = Pickle.string("city")

val address: Pickle[Address] = {
	val realAddress: Pickle[Address] = street.quad(number, zipCode, city)
		.wrap[Address]({ case (s, n, z, c) => RealAddress(s, n, z, c) })({ case RealAddress(s, n, z, c) => (s, n, z, c) })
	val antwoordnummerAddress: Pickle[Address] = antwoordnummer.triple(zipCode, city)
		.wrap[Address]({ case (a, z, c) => AntwoordnummerAddress(a, z, c) })({ case AntwoordnummerAddress(a, z, c) => (a, z, c) })
	val altAddress: Pickle[Address] = Pickle.alt[Address](Array(realAddress, antwoordnummerAddress)) {
		case _: RealAddress => 0
		case _: AntwoordnummerAddress => 1
	}
	Pickle.inside("address")(altAddress)
}

val name: Pickle[String] = Pickle.string("name")
val age: Pickle[Int] = Pickle.attribute("age").seq[Int](_.toString)(s => Pickle.lift(s.toInt))
val prefixedAge: Pickle[Int] = Pickle.attribute("prefix", "age").seq[Int](_.toString)(s => Pickle.lift(s.toInt))
val mail: Pickle[Option[String]] = Pickle.string("mail").maybe
val person: Pickle[Person] = name.quad(age.pair(prefixedAge), address, mail)
	.wrap(x => Person(x._1, x._2._1, x._3, x._4))({ case Person(n, a, ad, m) => (n, (a, a), ad, m) })

val pp = new PrettyPrinter(80, 4)

val res1 = person.pickle(obj1, <person/>)
pp.format(res1)
//val res2 = person.pickle(obj2, <person/>)
//pp.format(res2)
