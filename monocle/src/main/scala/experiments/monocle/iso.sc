import monocle.Iso

import scala.language.postfixOps

case class Point(x: Double, y: Double)
case class Player(name: String, position: Point)
type Players = List[Player]

val point = Point(0, 0)

val pointIso = Iso[Point, (Double, Double)](point => (point.x, point.y))(Point.tupled)

// object to tuple
pointIso.get(point)

// tuple to object
pointIso.reverseGet((0, 0))
pointIso((0, 0))

pointIso.modify { case (x, y) => (1 + x, 2 + y) }(point)

def listToStream[A] = Iso[List[A], Stream[A]](_.toStream)(_.toList)
listToStream.get(List(1, 2, 3))

val stringToList = Iso[String, List[Char]](_.toList)(_.mkString(""))
stringToList.get("abc")

listToStream[Int]
  .modify(_
    .map(i => { println(s"map1: $i"); i + 1 })
    .map(i => { println(s"map2: $i"); i * 2 })
  )
  .apply(List(1, 2, 3))
