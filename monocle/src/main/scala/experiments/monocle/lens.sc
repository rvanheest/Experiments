import monocle.Lens

import scala.language.postfixOps

type XCoord = Double
type YCoord = Double
case class Position(x: XCoord, y: YCoord)
case class Player(position: Position)

val position = Position(0, 0)
val player = Player(position)

val xCoordLens = Lens[Position, XCoord](_.x)(x => p => p.copy(x = x))
val yCoordLens = Lens[Position, YCoord](_.y)(y => p => p.copy(y = y))
val positionLens = Lens[Player, Position](_.position)(pos => player => player.copy(position = pos))

xCoordLens.get(position)
xCoordLens.set(1)(position)

val composedPositionXCoordLens = positionLens.composeLens(xCoordLens)
val composedPositionYCoordLens = positionLens.composeLens(yCoordLens)

composedPositionXCoordLens.modify(1 +)(player)
