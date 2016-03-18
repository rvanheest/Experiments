package experiments.typeclasses.serialize.v9

import scala.language.implicitConversions

object Serialization {

	case class Person(name: String, age: Int)

	case class Restaurant(name: String, brunch: Boolean)

	def serialize[T](t: T)(implicit s: Serializable[T]) = s.ser(t)

	trait Serializable[T] {
		def ser(t: T): String
	}

	implicit def PersonIsSerializable = {
		new Serializable[Person] {
			def ser(p: Person): String = s"Person(${p.name}, ${p.age})"
		}
	}

	implicit def RestaurantIsSerializable() {
		new Serializable[Restaurant] {
			def ser(r: Restaurant): String = s"Restaurant(${r.name}, ${r.brunch})"
		}
	}

	implicit def ListIsSerializable[T: Serializable] = {
		new Serializable[List[T]] {
			def ser(xs: List[T]) = {
				xs.map(x => serialize(x)).mkString("List(", ", ", ")")
			}
		}
	}

	implicit def addSerializable[T: Serializable](t: T): Object {def serialize: String} = {
		new {
			def serialize: String = implicitly[Serializable[T]].ser(t)
		}
	}
}
