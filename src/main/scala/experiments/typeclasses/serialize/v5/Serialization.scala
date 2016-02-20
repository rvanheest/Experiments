package experiments.typeclasses.serialize.v5

object Serialization {

	case class Person(name: String, age: Int)
	case class Restaurant(name: String, brunch: Boolean)

	def serialize[T](t: T)(implicit s: Serializable[T]) = s.serialize(t)

	trait Serializable[T] {
		def serialize(t: T): String
	}

	implicit object PersonIsSerializable extends Serializable[Person] {
		def serialize(p: Person): String = s"Person(${p.name}, ${p.age})"
	}

	implicit object RestaurantIsSerializable extends Serializable[Restaurant] {
		def serialize(r: Restaurant): String = s"Restaurant(${r.name}, ${r.brunch})"
	}
}
