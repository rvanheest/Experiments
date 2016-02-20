import experiments.typeclasses.serialize.v5.Serialization._

val p = Person("Foo", 40)
serialize(p)

val r = Restaurant("Bar", brunch = true)
serialize(r)
