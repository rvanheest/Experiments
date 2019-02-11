package experiments.dlist

object DListDemo extends App {

  println(DList.of(1, 2, 3).toList)
  println(DList.fromList(List(1, 2, 3)).toList)

  println(DList.empty.toList)

  println(DList.singleton(0).toList)

  println((0 :: 1 :: 2 :: 3 :: DList.empty).toList)

  println((DList.of(1, 2, 3) ::: DList.of(4, 5, 6)).toList)
  println((DList.of(1, 2, 3) append DList.of(4, 5, 6)).toList)

  println(DList.concat(List(
    DList.of(1, 2, 3),
    DList.of(4, 5, 6),
    DList.of(7, 8, 9)
  )).toList)

  println(DList.fill(6)("a").toList)

  println(DList.of(1, 2, 3).list(DList.of(4))((x, dl) => dl :+ x).toList)
  println(DList.empty[Int].list(DList.of(4))((x, dl) => dl :+ x).toList)

  println(DList.of(1, 2, 3).head)
  println(DList.of().headOption)
  println(DList.of(1, 2, 3).headOption)

  println(DList.of(1, 2, 3).foldRight(0)(_ + _))

  println(DList.of(1, 2, 3).map(1 +).toList)

  println((for {
    a <- DList.of(1, 2, 3)
    b <- DList.of(4, 5, 6)
  } yield (a, b)).toList)
}
