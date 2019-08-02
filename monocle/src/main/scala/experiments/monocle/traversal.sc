import monocle.{Lens, Traversal}

import scala.language.{higherKinds, postfixOps}
import scalaz.std.list._

val trav = Traversal.fromTraverse[List, Int]

val xs = List(1, 2, 3)
val empty = List.empty[Int]

trav.set(1)(xs)

trav.modify(1 +)(xs)

trav.getAll(xs)

trav.headOption(xs)

trav.find(_ > 3)(xs)

trav.all(_ > 3)(xs)

case class Position(x: Double, y: Double)
val position = Position(3, 0)

val pointTraversal = Traversal.apply2[Position, Double](_.x, _.y)((x, y, position) => position.copy(x = x, y = y))
pointTraversal.set(5)(position)
pointTraversal.modify(1 +)(position)

case class Player(position: Position)
val player = Player(position)

val playerLens = Lens[Player, Position](_.position)(pos => _.copy(position = pos))
val composed: Traversal[Player, Double] = playerLens.composeTraversal(pointTraversal)
composed.modify(1 +)(player)

type Players = List[Player]
val players = List(Player(Position(0, 0)), Player(Position(1, 2)))

val playersTraversal: Traversal[Players, Player] = Traversal.fromTraverse[List, Player]
val complexComposed1: Traversal[Players, Double] = playersTraversal.composeLens(playerLens).composeTraversal(pointTraversal)
val complexComposed2: Traversal[Players, Double] = playersTraversal.composeTraversal(playerLens.composeTraversal(pointTraversal))

complexComposed1.modify(1 +)(players)
complexComposed2.modify(1 +)(players)
