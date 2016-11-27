import monadics.instances.list._
import monadics.instances.monoids.values._

import scala.language.postfixOps

val f: Int => String = _.toString
val g: Int => String = i => s"g: $i"

val l = List(1, 2, 3)
val lf = List(f, g)
val n = List.empty[Int]
val lnf = List.empty[Int => String]

l.as("foo")
n.as("foo")

l.void
n.void

l.zipWith(i => s"$i$i$i")
n.zipWith(i => s"$i$i$i")

lf <*> l
lf <*> n
lnf <*> l
lnf <*> n

l.foldMap(i => 2L * i)
l.map(2 >=).all
l.map(2 >=).any
