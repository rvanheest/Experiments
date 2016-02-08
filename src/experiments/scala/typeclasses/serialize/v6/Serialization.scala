package experiments.scala.typeclasses.serialize.v6

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

	implicit def RestaurantIsSerializable {
		new Serializable[Restaurant] {  
			def ser(r: Restaurant): String = s"Restaurant(${r.name}, ${r.brunch})"
		}
	}
	
	implicit def ListIsSerializable[T](implicit ev: Serializable[T]) = {
		new Serializable[List[T]] {
			def ser(xs: List[T]) = {
				// serialize(x) == serialize(x)(ev)
				xs.map(x => serialize(x)).mkString("List(", ", ", ")")
			}
		}
	}
}