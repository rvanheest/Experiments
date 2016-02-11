package experiments.scala.typeclasses.serialize.v2

object Serialization {

	trait Serializable {
		def serialize: String
	}
	case class Person(name: String, age: Int) extends Serializable {
		def serialize: String = s"Person($name, $age)"
	}
	case class Restaurant(name: String, brunch: Boolean) extends Serializable {
		def serialize: String = s"Restaurant($name, $brunch)"
	}

	
}