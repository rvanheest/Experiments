package experiments.predicates

import scala.language.implicitConversions

case class Predicate[T](predicate: T => Boolean) extends AnyVal {

  def apply(t: T): Boolean = predicate(t)

  def lift(op: (Boolean, Boolean) => Boolean)(that: Predicate[T]): Predicate[T] = {
    Predicate(t => op(predicate(t), that(t)))
  }

  def unary_! : Predicate[T] = Predicate(!predicate(_))

  def and(that: Predicate[T]): Predicate[T] = {
    lift(_ && _)(that)
  }

  def or(that: Predicate[T]): Predicate[T] = {
    lift(_ || _)(that)
  }

  def xnor(that: Predicate[T]): Predicate[T] = {
    lift(_ == _)(that)
  }

  def nand(that: Predicate[T]): Predicate[T] = {
    !(this and that)
  }

  def nor(that: Predicate[T]): Predicate[T] = {
    !(this or that)
  }

  def xor(that: Predicate[T]): Predicate[T] = {
    !(this xnor that)
  }

  def ->(that: Predicate[T]): Predicate[T] = {
    !this or that
  }

  def <->(that: Predicate[T]): Predicate[T] = {
    (this -> that) and (that -> this)
  }
}

object Predicate {

  implicit def funcToPred[T](predicate: T => Boolean): Predicate[T] = Predicate(predicate)
  implicit def predToFunc[T](predicate: Predicate[T]): T => Boolean = predicate.predicate

  def of[T](b: => Boolean): Predicate[T] = Predicate(_ => b)

  def test[T](predicate: Predicate[T])(values: T*): (Seq[T], Seq[T]) = {
    values.partition(predicate)
  }
}

object Demo extends App {

  import Predicate._

  val p1 = (i: Int) => i > 0
  val p2 = (i: Int) => i % 2 == 0

  val p3 = !p1
  val p4 = p1 and p2
  val p5 = p1 or p2
  val p6 = p1 xor p2
  val p7 = p1 nor p2
  val p8 = p1 nand p2
  val p9 = p1 xnor p2
  val p10 = p1 -> p2
  val p11 = p1 <-> p2


}
