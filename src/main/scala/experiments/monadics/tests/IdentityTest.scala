package experiments.monadics.tests

import experiments.monadics.instances.Identity

import scala.language.postfixOps

object IdentityTest extends App {

  val id1 = Identity(5)
  val id2 = Identity(3.0)
  val idf = Identity[Double => Int](_ toInt)

  println(id1.map(2 *))

  println(idf <*> id2)
  println(id1 *> id2)
  println(id1 <* id2)

  println(id1.flatMap(i => id2.map(i +)))
  println(for { i <- id1; j <- id2 } yield i * j)
}
