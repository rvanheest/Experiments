package experiments.transformers.test

import experiments.transformers.ListT
import experiments.transformers._

object ListTTest extends App {

	val opt1 = Option(List(1, 2, 3, 4))
	val opt2 = Option(List(5, 6, 7, 8))

	val lt1 = ListT(opt1)
	val lt2 = ListT(opt2)

	println(lt1.map(1 +))

	println(lt1.filter(i => i % 2 == 0))

	println(lt1.flatMap(i => lt2.map(j => i + j)))
}
