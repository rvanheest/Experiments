package experiments.scala.typeclasses.serialize.v3

object Serialization {

	case class Person(name: String, age: Int)
	case class Restaurant(name: String, brunch: Boolean)

	def serialize(x: Any) = {
		x match {
			case Person(name, age) => s"Person($name, $age)"
			case Restaurant(name, brunch) => s"Restaurant($name, $brunch)"
			case _ => ???
		}
	}
	
	def serializeToJSON(x: Any) = ???
}