import experiments.typeclasses.serialize.v8.Serialization._

val p1 = Person("Foo", 40)
val p2 = Person("Bar", 30)
val list = List(p1, p2)

p1.serialize
p2.serialize
list.serialize
