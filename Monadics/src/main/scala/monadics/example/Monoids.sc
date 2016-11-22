import monadics.instances.values._
import monadics.instances.option._
import monadics.instances.map._
import monadics.instances.list._
import monadics.instances.{NonEmptyList, Product, Sum}

val sum1 = Sum(4)
val sum2 = Sum(6)
val product1 = Product(7)
val product2 = Product(9)

val sum3 = sum1 + sum2
val product3 = Product(sum3.sum) * product1
Sum(product3.product) + sum2

val opt1 = Option("foo")
val opt2 = Option("bar")
val none = Option.empty[String]

none.combine(none)
opt1.combine(none)
none.combine(opt2)
opt1.combine(opt2)

val nel1 = NonEmptyList(1, 2, 3, 4)
val nel2 = NonEmptyList(5)

nel1 ++ nel2
nel1.map(i => i * 2)
nel1.flatMap(i => nel2.map(j => (i, j)))

val map1 = Map("even" -> List(2, 4, 6, 8))
val map2 = Map("even" -> List(10, 12), "odd" -> List(1, 3, 5))
map1.combine(map2)

val map3 = Map("a" -> Option(List(1, 2, 3)), "b" -> Option(List(4, 5, 6)))
val map4 = Map("a" -> Option(List(7, 8, 9)))
val map5 = Map("a" -> Option.empty[List[Int]], "c" -> Option.empty)
map3.combine(map4).combine(map5)
