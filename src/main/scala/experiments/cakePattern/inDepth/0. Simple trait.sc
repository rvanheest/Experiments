trait SimpleTrait {
	def addOne(x: Int): Int = x + 1
}
class SimpleClass extends SimpleTrait

val addOne1 = new SimpleClass
addOne1.addOne(1)

val addOne2 = new SimpleTrait {}
addOne2.addOne(2)
