import monadics.instances.NonEmptyList

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
