import scala.language.{ higherKinds, postfixOps }
import LensLib.Lens._

object LensLib {

  case class Store[W, P](get: P, set: P => W)
  trait Lens[W, P] { self =>
    def run(w: W): Store[W, P]

    def over(f: P => P)(w: W): W = {
      val store = this.run(w)
      store.set(f(store.get))
    }

    def andThen[Q](other: Lens[P, Q]): Lens[W, Q] = {
      new Lens[W, Q] {
        def run(w: W): Store[W, Q] = {
          val get = other.run(self.run(w).get).get
          val store = self.run(w)
          val set = (q: Q) => store.set(other.run(store.get).set(q))
          Store(get, set)
        }
      }
    }
  }
  object Lens {
    def lens[W, P](get: W => P)(set: (W, P) => W): Lens[W, P] = {
      new Lens[W, P] {
        def run(w: W): Store[W, P] = Store(get(w), set.curried(w))
      }
    }
  }
}

case class Point(x: Double, y: Double)
case class Player(position: Point)

val player1 = Player(Point(0, 0))

val xLens = lens[Point, Double](_.x)((p, x) => p.copy(x = x))
val positionLens = lens[Player, Point](_.position)((pl, pt) => pl.copy(position = pt))

val xPosFromPlayer = positionLens.andThen(xLens)

xPosFromPlayer.run(player1).get
xPosFromPlayer.run(player1).set(1.0)
xPosFromPlayer.over(2 +)(player1)
