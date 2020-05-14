package experiments.monadics.tests

import experiments.monadics.instances.{Product, Sum}

object SemiGroupsAndMonoidsTest extends App {

	val sum0 = Sum.empty[Int]
	val sum1 = Sum(1)
	val sum2 = Sum(2)

	println(sum0 + sum1)
	println(sum1 + sum2)

	val product0 = Product.empty[Int]
	val product1 = Product(1)
	val product2 = Product(2)

	println(product0 * product1)
	println(product1 * product1)
	println(product1 * product2)
	println(product2 * product2)
}
