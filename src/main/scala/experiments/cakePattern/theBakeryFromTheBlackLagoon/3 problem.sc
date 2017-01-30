trait A {
	val foo: String
}
trait B extends A {
	val bar: String = foo + "World" // this causes "nullWorld"
				            						  // make bar a lazy val or make foo a def
}
class C extends B {
	val foo: String = "Hello"

	println(bar)
}
new C
