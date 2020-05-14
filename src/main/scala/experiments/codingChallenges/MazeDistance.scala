package experiments.codingChallenges

import scala.collection.mutable

object MazeDistance extends App {

  sealed abstract class Tile {
    def distance: Int

    def mkString: String
  }
  case class Cell(override val distance: Int) extends Tile {
    override def mkString: String = distance.toString
  }
  case object UnvisitedCell extends Tile {
    override def distance: Int = throw new IllegalStateException("no distance for unvisited cell")

    override def mkString: String = "_"
  }
  case object Wall extends Tile {
    override def distance: Int = throw new IllegalStateException("no distance for wall")

    override def mkString: String = "W"
  }
  case object Gate extends Tile {
    override def distance: Int = 0

    override def mkString: String = "G"
  }

  type Coordinate = (Int, Int)
  type Maze = Array[Array[Tile]]

  implicit class MazePrinter(val maze: Maze) extends AnyVal {
    def mkString: String = maze.map(_.map(_.mkString).mkString(" ")).mkString("\n")
  }

  val maze: Maze = Array(
    // @formatter:off
    Array(UnvisitedCell, Wall,          Gate,          UnvisitedCell),
    Array(UnvisitedCell, UnvisitedCell, UnvisitedCell, Wall),
    Array(UnvisitedCell, Wall,          UnvisitedCell, Wall),
    Array(Gate,          Wall,          UnvisitedCell, UnvisitedCell)
    // @formatter:on
  )
  val gates = Seq(
    0 -> 2,
    3 -> 0
  )

  println(maze.mkString)
  println
  println(calculateDistances(maze, gates).mkString)

  def calculateDistances(maze: Maze, gateCoords: Seq[Coordinate]): Maze = {
    val queue: mutable.Queue[Coordinate] = mutable.Queue(gateCoords: _*)

    while (queue.nonEmpty) {
      val coord @ (x, y) = queue.dequeue()
      val neighbourDistance = maze(x)(y).distance + 1
      for (neighbour @ (nX, nY) <- getUnvisitedNeighbours(maze, coord)) {
        maze(nX)(nY) = Cell(neighbourDistance)
        queue.enqueue(neighbour)
      }
    }

    maze
  }

  private def getUnvisitedNeighbours(maze: MazeDistance.Maze, coordinate: Coordinate): Seq[Coordinate] = {
    val (x, y) = coordinate
    if (x < 0 || x >= maze.length || y < 0 || y >= maze(x).length)
      throw new IllegalArgumentException(s"coordinate $coordinate out of bound")

    Seq[(Int, Int) => Coordinate](
      (x, y) => (x + 1, y),
      (x, y) => (x - 1, y),
      (x, y) => (x, y + 1),
      (x, y) => (x, y - 1)
    )
      .map(_ (x, y))
      .withFilter { case (nX, _) => nX >= 0 }
      .withFilter { case (nX, _) => nX < maze.length }
      .withFilter { case (_, nY) => nY >= 0 }
      .withFilter { case (nX, nY) => nY < maze(nX).length }
      .withFilter { case (nX, nY) =>
        maze(nX)(nY) match {
          case UnvisitedCell => true
          case _ => false
        }
      }
      .map(identity)
  }
}
