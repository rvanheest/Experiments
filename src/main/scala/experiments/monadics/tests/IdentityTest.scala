package experiments.monadics.tests

import experiments.monadics.instances.Identity
import experiments.monadics._

object IdentityTest extends App {

  val id1 = new Identity(5)
  val id2 = new Identity(3.0)

  println(id1.map(2 *))
  println(id1.flatMap(i => id2.map(i +)))
  println(for { i <- id1; j <- id2 } yield i * j)
}
