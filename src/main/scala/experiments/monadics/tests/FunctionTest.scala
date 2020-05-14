package experiments.monadics.tests

import experiments.monadics.instances.Function

object FunctionTest extends App {

	val id = Function.identity[Int]
	val f = Function[Int, Double](i => i.toDouble)
	val g = Function[Double, String](d => d.toString)
	val h = g.compose(f)
	val k = f.andThen(g)

	println(id(5))
	println(h(6))
	println(k(7))
}
