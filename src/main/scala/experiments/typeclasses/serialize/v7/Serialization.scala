package experiments.typeclasses.serialize.v7

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
	
	implicit def ListIsSerializable[T : Serializable] = {
		// (implicit ev: Serializable[T]) is shortened to [T : Serializable]
		new Serializable[List[T]] {
			def ser(xs: List[T]) = {
				// serialize(x) uses the evidence
				xs.map(x => serialize(x)).mkString("List(", ", ", ")")
			}
		}
	}
}
