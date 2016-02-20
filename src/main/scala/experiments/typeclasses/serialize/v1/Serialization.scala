package experiments.typeclasses.serialize.v1

object Serialization {

	case class Person(name: String, age: Int)
	case class Restaurant(name: String, brunch: Boolean)

	def serialize(p: Person) = s"Person(${p.name}, ${p.age})"
	def serialize(r: Restaurant) = s"Restaurant(${r.name}, ${r.brunch})"
}
