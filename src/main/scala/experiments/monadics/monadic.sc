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
j1.map(i => i + 10)
j1.map(i => j2.map(j => j + i))
j1.map(i => n)
n.map(i => i + 10)
n.map(i => j1)

// applicative
jf <*> j1
jf <*> n
j1 *> j2
j1 <* j2

// monad
j1.flatMap(i => j2.map(j => j + i))
j1.flatMap(i => n.map(j => j + i))
n.flatMap(i => j2.map(j => j + i))
for { i <- j1; j <- j2 } yield j + i

// monadplus
j1.mplus(j2)
j1.mplus(n)
n.mplus(j2)
n.mplus(n)
