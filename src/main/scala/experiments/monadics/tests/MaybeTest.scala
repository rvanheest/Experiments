package experiments.monadics.tests

import experiments.monadics.instances.{Maybe, Sum}

import scala.language.postfixOps

object MaybeTest extends App {

  val j1 = Maybe(1)
  val j2 = Maybe(2)
  val jf = Maybe((a: Int) => a.toDouble)
  val n = Maybe.empty[Int]

  val sum1 = Sum(5)
  val sum2 = Sum(3)
  val sum3 = Sum.empty[Int]

  println("MONOID")
  println(Maybe(sum3).append(Maybe(sum3)))
  println(Maybe(sum2).append(Maybe(sum3)))
  println(Maybe(sum3).append(Maybe(sum1)))
  println(Maybe(sum1).append(Maybe(sum2)))

  // functor
  println("\nFUNCTOR")
  println(j1.map(10 +))
  println(j1.map(i => j2.map(i +)))
  println(j1.map(_ => n))
  println(n.map(10 +))
  println(n.map(_ => j1))
  println(jf.map(j1.map))

  // applicative
  println("\nAPPLICATIVE")
  println(jf <*> j1)
  println(jf <*> n)
  println(j1 <**> jf)

  // alternative
  println("\nALTERNATIVE")
  println(j1.orElse(j2))
  println(j2.orElse(j1))
  println(j1.orElse(n))
  println(n.orElse(j1))
  println(n.orElse(n))
  println(j1.getOrElse(-1))
  println(n.getOrElse(-1))
  println(j1.maybe)
  println(n.maybe)

  // monad
  println("\nMONAD")
  println(j1 >>= (i => j2.map(i +)))
  println(j1 >>= (i => n.map(i +)))
  println(n >>= (i => j2.map(i +)))
  println(n >>= (i => n.map(i +)))
  println(jf >>= (f => j1.map(f(_))))

  // monad for-comprehension
  println("\nMONAD FOR-COMPREHENSION")
  println(for {i <- j1; j <- j2} yield i + j)
  println(for {i <- j1; j <- n} yield i + j)
  println(for {i <- n; j <- j1} yield i + j)
  println(for {i <- n; j <- n} yield i + j)
  println(for {f <- jf; j <- j1} yield f(j))

  // monadplus
  println("\nMONADPLUS")
  println(j1.filter(_ % 2 == 0))
  println(j2.filter(_ % 2 == 0))
  println(n.filter(_ % 2 == 0))
}
