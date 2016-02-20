import experiments.typeclasses.serialize.v4.Serialization._

val p = Person("Foo", 40)
serialize(p, PersonIsSerializable)

val r = Restaurant("Bar", brunch = true)
serialize(r, RestaurantIsSerializable)

