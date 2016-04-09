import experiments.monadics.instances.Maybe
import experiments.monadics.instances._
import experiments.monadics._

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
