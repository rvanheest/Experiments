package experiments.transformers.test

import experiments.transformers.OptionT
import experiments.transformers._

import scala.language.postfixOps

object OptionTTest extends App {

	val list1 = Option(1) :: Option(2) :: Option(3) :: Option(4) :: Nil
	val list2 = Option(5) :: Option(6) :: Option(7) :: Option(8) :: Nil

	val ot1 = OptionT(list1)
	val ot2 = OptionT(list2)

	println(ot1.map(1 +))

	println(ot1.filter(i => i % 2 == 0))

	println(ot1.flatMap(i => ot2.map(j => i + j)))
}
