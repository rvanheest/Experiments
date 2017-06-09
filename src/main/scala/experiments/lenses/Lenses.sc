import scala.language.{ higherKinds, postfixOps }
import LensLib.Lens._

object LensLib {

  case class Store[W, P](get: P, set: P => W)
  trait Lens[W, P] extends (W => Store[W, P]) { self =>
    def over(f: P => P)(w: W): W = {
      val store = this(w)
      store.set(f(store.get))
    }

    def andThen[Q](other: Lens[P, Q]): Lens[W, Q] = {
      new Lens[W, Q] {
        override def apply(w: W): Store[W, Q] = {
          val get = other(self(w).get).get
          val store = self(w)
          val set = (q: Q) => store.set(other(store.get).set(q))
          Store(get, set)
        }
      }
    }

    def compose[Q](other: Lens[Q, W]): Lens[Q, P] = {
      new Lens[Q, P] {
        override def apply(q: Q): Store[Q, P] = {
          val get = self(other(q).get).get
          val store = other(q)
          val set = (p: P) => store.set(self(store.get).set(p))
          Store(get, set)
        }
      }
    }
  }
  object Lens {
    def lens[W, P](get: W => P)(set: (W, P) => W): Lens[W, P] = {
      new Lens[W, P] {
        override def apply(w: W): Store[W, P] = Store(get(w), set.curried(w))
      }
    }
  }
}

case class Point(x: Double, y: Double)
case class Player(position: Point)
case class Players(list: List[Player])

val player1 = Player(Point(0, 0))

val xLens = lens[Point, Double](_.x)((p, x) => p.copy(x = x))
val positionLens = lens[Player, Point](_.position)((pl, pt) => pl.copy(position = pt))

val xPosFromPlayer = positionLens.andThen(xLens)

xPosFromPlayer(player1).get
xPosFromPlayer(player1).set(1.0)
xPosFromPlayer.over(2 +)(player1)
