import monadics.instances.option._
import monadics.instances.map._
import monadics.instances.list._

val map1 = Map("even" -> List(2, 4, 6, 8))
val map2 = Map("even" -> List(10, 12), "odd" -> List(1, 3, 5))
map1.combine(map2)

val map3 = Map("a" -> Option(List(1, 2, 3)), "b" -> Option(List(4, 5, 6)))
val map4 = Map("a" -> Option(List(7, 8, 9)))
val map5 = Map("a" -> Option.empty[List[Int]], "c" -> Option.empty)
map3.combine(map4).combine(map5)
