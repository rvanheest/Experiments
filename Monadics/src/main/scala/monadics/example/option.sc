import monadics.instances.option._
import monadics.instances.list._
import monadics.instances.monoids.values._

import scala.language.postfixOps

val s1 = Option(1)
val s2 = Option(2)
val f1: Option[Int => String] = Option(_.toString)
val f2: Option[Int => String => Int] = Option(i => s => s"$i$s".length)
val n1 = Option.empty[Int]
val n2 = Option.empty[Int]

s1.combine(s2) // Some(3)
s2.combine(n1) // Some(2)
n1.combine(s1) // Some(1)
n1.combine(n2) // None

s1.as("abc")    // Some("abc")
n1.as("abc")    // None
s1.void         // Some(())
n1.void         // None
s1.zipWith(2 *) // Some((1, 2))
n1.zipWith(2 *) // None

f1 <*> s1       // Some("1")
f2 <*> s1 <*> s2.as("abc") // Some(4)

Option(1).traverse[List, Int](i => List(i, i + 1, i + 2))
Option(List(1, 2, 3)).sequence
