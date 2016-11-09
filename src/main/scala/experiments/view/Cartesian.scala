package experiments.view

case class Pole(magnitude: Double, argument: Double)
case class Cartesian(x: Double, y: Double)

object Cartesian extends View[Pole, Cartesian] {

  implicit def in(pole: Pole): Cartesian =
    Cartesian(pole.magnitude * math.cos(pole.argument), pole.magnitude * math.sin(pole.argument))

  implicit def out(cart: Cartesian): Pole = {
    val Cartesian(x, y) = cart
    Pole(math.sqrt(x * x + y * y), math.atan2(x, y))
  }
}

object ComplexNumbers extends App {

  import Cartesian._

  def add(cart1: Cartesian, cart2: Cartesian): Cartesian = {
    val Cartesian(x1, y1) = cart1
    val Cartesian(x2, y2) = cart2
    Cartesian(x1 + x2, y1 + y2)
  }

  def multiply(pole1: Pole, pole2: Pole): Pole = {
    val Pole(m1, a1) = pole1
    val Pole(m2, a2) = pole2
    Pole(m1 * m2, a1 * a2)
  }

  val c1 = Cartesian(1, 3)
  val c2 = Pole(5, 0.64)

  println(add(c1, c2))
  println(multiply(c1, c2))
}
