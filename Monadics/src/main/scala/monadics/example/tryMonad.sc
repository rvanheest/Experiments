import monadics.instances.tryMonad._

import scala.util.{Failure, Try}

val g: Int => String = _.toString
val s = Try(1)
val sg = Try(g)
val f = Failure[Int](new Exception("foobar"))
val fg = Failure[Int => String](new Exception("foo"))

s.as("abc")
f.as("abc")

s.void
f.void

s.zipWith(i => s"$i$i$i")
f.zipWith(i => s"$i$i$i")

sg <*> s
sg <*> f
fg <*> s
fg <*> f
