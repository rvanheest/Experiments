import monadics.ScalaMonoids._
import monadics.instances.{NonEmptyList, Product, Sum}

val sum1 = Sum(4)
val sum2 = Sum(6)
val product1 = Product(7)
val product2 = Product(9)

val sum3 = sum1 + sum2
val product3 = Product(sum3.sum) * product1
Sum(product3.product) + sum2

//val list1 = List(1, 2, 3)
//val list2 = List(4, 5, 6)
//
//list1.append(list2)

val string1 = "abc"
val string2 = "def"

string1.append(string2)

val opt1 = Option("foo")
val opt2 = Option("bar")
val none = Option.empty[String]

none.append(none)
opt1.append(none)
none.append(opt2)
opt1.append(opt2)

val nel1 = NonEmptyList(1, 2, 3, 4)
val nel2 = NonEmptyList(5)

nel1 ++ nel2
nel1.map(i => i * 2)
nel1.flatMap(i => nel2.map(j => (i, j)))
