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
