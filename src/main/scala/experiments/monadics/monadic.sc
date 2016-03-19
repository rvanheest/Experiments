import experiments.monadics.instances.Maybe
import experiments.monadics.instances._
import experiments.monadics._

def maybe[A](empty: Boolean)(value: => A): Maybe[A] = {
  if (empty) Maybe.empty
  else Maybe.apply(value)
}

val j1 = maybe(empty = false)(1)
val j2 = maybe(empty = false)(2)
val n = maybe(empty = true)(1)

j1.mappend(j2)
j1.mappend(n)
n.mappend(j2)
n.mappend(n)

// functor
j1.map(i => i + 10)
j1.map(i => j2.map(j => j + i))
j1.map(i => n)
n.map(i => i + 10)

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
