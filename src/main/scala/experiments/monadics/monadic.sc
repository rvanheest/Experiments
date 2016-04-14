import experiments.monadics.instances.Maybe
import experiments.monadics.instances._
import experiments.monadics._

import scala.language.postfixOps

val j1 = Maybe(1)
val j2 = Maybe(2)
val jf = Maybe((a: Int) => a.toDouble)
val n = Maybe.empty[Int]

// monoid
j1.mappend(j2)
j1.mappend(n)
n.mappend(j2)
n.mappend(n)

// functor
j1.map(10 +)
j1.map(i => j2.map(i +))
j1.map(_ => n)
n.map(10 +)
n.map(_ => j1)
jf.map(j1.map)

// applicative
jf <*> j1
jf <*> n
j1 *> j2
j1 *> n
j1 <* j2
j1 <* n
j1 <**> jf
j1.liftA(_.toDouble)
n.liftA(_.toDouble)
j1.liftA2(j2)(_ + _)
n.liftA2(j2)(_ + _)
j1.liftA2(n)(_ + _)
n.liftA2(n)(_ + _)

// alternative
j1 <|> j2
j2 <|> j1
j1 <|> n
n <|> j1
n <|> n

// monad
j1 >>= (i => j2.map(i +))
j1 >>= (i => n.map(i +))
n >>= (i => j2.map(i +))
n >>= (i => n.map(i +))
jf >>= (f => j1.map(f(_)))

j1 >> j2
j1 >> n
j1 >> jf
n >> j1
n >> n

j1 << j2
j1 << n
j1 << jf
n << j1
n << n

j1.liftM(10 +)
jf.liftM(_(2))
n.liftM(10 +)

j1.liftM2(j2)(_ + _)
j1.liftM2(n)(_ + _)
n.liftM2(j1)(_ + _)
n.liftM2(n)(_ + _)

j1.liftM3(j2, jf)((a, b, f) => f(a + b))
j1.liftM3(n, jf)((a, b, f) => f(a + b))
n.liftM3(j2, jf)((a, b, f) => f(a + b))
n.liftM3(n, jf)((a, b, f) => f(a + b))
j1.liftM3(j2, n)((a, b, c) => a + b + c)
n.liftM3(j2, n)((a, b, c) => a + b + c)
j1.liftM3(n, n)((a, b, c) => a + b + c)
n.liftM3(n, n)((a, b, c) => a + b + c)

// monad for-comprehension
for { i <- j1; j <- j2 } yield i + j
for { i <- j1; j <- n } yield i + j
for { i <- n; j <- j1 } yield i + j
for { i <- n; j <- n } yield i + i
for { f <- jf; j <- j1 } yield f(j)

// monadplus
j1.mplus(j2)
j2.mplus(j1)
j1.mplus(n)
n.mplus(j1)
n.mplus(n)

val id1 = new Identity(5)
val id2 = new Identity(3.0)

id1.map(2 *)
id1.flatMap(i => id2.map(i +))
for { i <- id1; j <- id2 } yield i * j
