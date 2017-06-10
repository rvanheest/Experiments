import scala.language.{higherKinds, postfixOps}
import Lens._

class Store[W, P](val get: P, val set: P => W)

object Store {
  def apply[W, P](get: P)(set: P => W): Store[W, P] = new Store[W, P](get, set)
}

trait Lens[W, P] extends (W => Store[W, P]) {
  self =>

  def modify(f: P => P)(w: W): W = {
    val store = self(w)
    store.set(f(store.get))
  }

  def andThen[Q](other: Lens[P, Q]): Lens[W, Q] = new Lens[W, Q] {
    override def apply(w: W): Store[W, Q] = {
      Store(other(self(w).get).get) {
        val store = self(w)
        (q: Q) => store.set(other(store.get).set(q))
      }
    }
  }

  def compose[Q](other: Lens[Q, W]): Lens[Q, P] = new Lens[Q, P] {
    override def apply(q: Q): Store[Q, P] = {
      Store(self(other(q).get).get) {
        val store = other(q)
        (p: P) => store.set(self(store.get).set(p))
      }
    }
  }

  def andThenList[PS, Q](other: Lens[PS, Q])(implicit ev: P <:< List[PS], ev2: List[PS] <:< P): Lens[W, List[Q]] = new Lens[W, List[Q]] {
    override def apply(w: W): Store[W, List[Q]] = {
      Store(self(w).get.map(other(_).get)) {
        val store = self(w)
        (qs: List[Q]) => store.set(store.get.zip(qs).map { case (oldP, q) => other(oldP).set(q) })
      }
    }
  }
}

object Lens {
  def lens[W, P](get: W => P)(set: (W, P) => W): Lens[W, P] = {
    new Lens[W, P] {
      override def apply(w: W): Store[W, P] = Store(get(w))(set.curried(w))
    }
  }
}

case class Point(x: Double, y: Double)

case class Player(position: Point)

case class Players(list: List[Player])

val player1 = Player(Point(0, 0))
val players = Players(List(player1))

val xLens = lens[Point, Double](_.x)((p, x) => p.copy(x = x))
val positionLens = lens[Player, Point](_.position)((pl, pt) => pl.copy(position = pt))
val playersLens = lens[Players, List[Player]](_.list)((pls, list) => pls.copy(list = list))

val xPosFromPlayer: Lens[Player, Double] = positionLens.andThen(xLens)
val xPosForEveryPlayer: Lens[Players, List[Double]] = playersLens.andThenList(xPosFromPlayer)

xPosFromPlayer(player1).get
xPosFromPlayer(player1).set(1.0)
xPosFromPlayer.modify(2 +)(player1)

xPosForEveryPlayer(players).get
xPosForEveryPlayer(players).set(List(1.0))
xPosForEveryPlayer.modify(_.map(2 +))(players)
