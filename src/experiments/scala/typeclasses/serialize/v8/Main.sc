package experiments.scala.typeclasses.serialize.v8

import experiments.scala.typeclasses.serialize.v8.Serialization._

object Main {
	val p1 = Person("Foo", 40)                //> p1  : experiments.scala.typeclasses.serialize.v8.Serialization.Person = Pers
                                                  //| on(Foo,40)
	val p2 = Person("Bar", 30)                //> p2  : experiments.scala.typeclasses.serialize.v8.Serialization.Person = Pers
                                                  //| on(Bar,30)
	val list = List(p1, p2)                   //> list  : List[experiments.scala.typeclasses.serialize.v8.Serialization.Person
                                                  //| ] = List(Person(Foo,40), Person(Bar,30))

	p1.serialize                              //> res0: String = Person(Foo, 40)
	p2.serialize                              //> res1: String = Person(Bar, 30)
	list.serialize                            //> res2: String = List(Person(Foo, 40), Person(Bar, 30))
}