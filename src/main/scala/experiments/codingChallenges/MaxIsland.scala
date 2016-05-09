package experiments.codingChallenges

import scala.collection.mutable

/**
  * Given a world consisting of water ('w') and land ('l'),
  * write a function that returns the size of the largest island
  */
object MaxIsland extends App {

  type World = List[List[Node]]
  type Coordinate = (Int, Int)

  object NodeType extends Enumeration {
    type NodeType = Value
    val LAND, WATER = Value
  }
  import NodeType._

  case class Node(nodeType: NodeType, coordinate: Coordinate)

  val visited = mutable.Set[Node]()

  def maxIslandSize(world: World): Int = {
    world.toStream
      .flatMap(_.toStream filter isNewLand)
      .map(islandSize(world, _))
      .foldLeft(0)(math.max)
  }

  def isNewLand(node: Node): Boolean = node.nodeType == LAND && !visited(node)

  def islandSize(world: World, start: Node): Int = {
    var score = 0
    val queue = mutable.Queue(start)

    do {
      val n = queue.dequeue()
      score += 1
      visited.add(n)
      listNeighbours(world, n.coordinate).filter(isNewLand).foreach(queue.enqueue(_))
    } while (queue.nonEmpty)

    score
  }

  def listNeighbours(world: World, coord: Coordinate): List[Node] = {
    val (r, c) = coord
    require(r >= 0 && r < world.length, s"row $r is out of scope")

    val row = world(r)
    require(c >= 0 && c < row.length, s"column $c is out of scope")

    val above = world.lift(r - 1).flatMap(_.lift(c))
    val below = world.lift(r + 1).flatMap(_.lift(c))
    val left = row.lift(c - 1)
    val right = row.lift(c + 1)

    List(above, right, below, left).flatten
  }

  def structToWorld(struct: List[List[Char]]): World = {
    struct.zipWithIndex
      .map { case (row, i) => row.zipWithIndex.map { case (c, j) => Node(charToNodeType(c), (i, j)) }}
  }

  def charToNodeType(c: Char) = c match {
    case 'w' => WATER
    case 'l' => LAND
    case _ => throw new IllegalArgumentException(s"character $c is not supported here!")
  }

  val world = List(
    List('w', 'w', 'l', 'l', 'w', 'w'),
    List('w', 'w', 'l', 'w', 'w', 'w'),
    List('w', 'w', 'l', 'l', 'w', 'l'),
    List('w', 'w', 'l', 'w', 'l', 'l', 'l'),
    List('l', 'w', 'w', 'w', 'w', 'l'),
    List('l', 'w', 'w', 'w', 'w', 'w')
  )

  println(maxIslandSize(structToWorld(world)))
}
