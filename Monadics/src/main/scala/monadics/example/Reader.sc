import monadics.instances.Reader
import monadics.instances.Reader.readerIsMonad

import scala.language.postfixOps

def times5(x: Int): Int = 5 * x
def plus3(x: Int): Int = 3 + x

val f: Reader[Int, Int] = new Reader(times5)
val g: Reader[Int, Int] = new Reader(plus3)
g.map(f).run(8)

def times2(x: Int): Int = 2 * x
def plus10(x: Int): Int = 10 + x

val h = new Reader(times2)(readerIsMonad)
val k = new Reader(plus10)(readerIsMonad)

val l = h.map[Int => Int](a => a + _).<*>(k)
l.run(3)

val m = for {
  a <- h
  b <- k
} yield a + b
m.run(3)
