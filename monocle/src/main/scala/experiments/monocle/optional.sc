import monocle.Optional

import scala.language.postfixOps

val headOfList = Optional[List[Int], Int](_.headOption)(x => xs => if (xs.isEmpty) Nil else x :: xs.tail)

val xs = List(1, 2, 3)
val empty = List.empty[Int]

headOfList.isEmpty(xs)
headOfList.isEmpty(empty)

headOfList.getOrModify(xs)
headOfList.getOrModify(empty)

headOfList.getOption(xs)
headOfList.getOption(empty)

headOfList.set(4)(xs)
headOfList.set(4)(empty)

headOfList.setOption(4)(xs)
headOfList.setOption(4)(empty)

headOfList.modify(1 +)(xs)
headOfList.modify(1 +)(empty)

headOfList.modifyOption(1 +)(xs)
headOfList.modifyOption(1 +)(empty)
