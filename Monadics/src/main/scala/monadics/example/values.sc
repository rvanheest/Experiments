import monadics.instances.NonEmptyList
import monadics.instances.monoids.{Product, Sum}

val sum1 = Sum(4)
val sum2 = Sum(6)
val product1 = Product(7)
val product2 = Product(9)

val sum3 = sum1 + sum2
val product3 = Product(sum3.sum) * product1
Sum(product3.product) + sum2

val nel1 = NonEmptyList(1, 2, 3, 4)
val nel2 = NonEmptyList(5)

nel1 ++ nel2
nel1.map(i => i * 2)
nel1.flatMap(i => nel2.map(j => (i, j)))
