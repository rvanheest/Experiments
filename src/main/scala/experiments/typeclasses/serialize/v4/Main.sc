package experiments.scala.typeclasses.serialize.v4

import experiments.scala.typeclasses.serialize.v4.Serialization._

object Main {
	val p = Person("Foo", 40)                 //> p  : experiments.scala.typeclasses.serialize.v4.Serialization.Person = Perso
                                                  //| n(Foo,40)
	serialize(p, PersonIsSerializable)        //> res0: String = Person(Foo, 40)
	
	val r = Restaurant("Bar", true)           //> r  : experiments.scala.typeclasses.serialize.v4.Serialization.Restaurant = R
                                                  //| estaurant(Bar,true)
	serialize(r, RestaurantIsSerializable)    //> res1: String = Restaurant(Bar, true)
}