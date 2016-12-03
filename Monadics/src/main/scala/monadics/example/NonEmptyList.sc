import monadics.instances.NonEmptyList
import monadics.instances.list._
import monadics.instances.monoids.Sum
import monadics.instances.option._

import scala.language.postfixOps

val f: Int => String = _.toString
val g: Int => String = i => s"g: $i"

val nel1 = NonEmptyList(1, 2, 3, 4)
val nel2 = NonEmptyList(5)
val nelf = NonEmptyList(f, g)

nel1 ++ nel2
nel1.map(i => i * 2)
nel1.as("abc")
nel1.void
nel1.zipWith(i => i * 2)
nelf <*> nel1
nel1.flatMap(i => nel2.map(j => (i, j)))

nel1.foldLeft(100)(_ - _)
List(1, 2, 3, 4).foldLeft(100)(_ - _)

nel1.foldRight(100)(_ - _)
List(1, 2, 3, 4).foldRight(100)(_ - _)

nel1.foldMap(Sum(_)).sum

nel1.traverse[Option, Int](Option(_))
nel1.traverse[List, Int](List(_, 5))
Option(nel1).traverse[List, Int](nel => nel.head :: nel.tail)

nel1.map(Option(_)).sequence
nel1.map(List(_, 5)).sequence
