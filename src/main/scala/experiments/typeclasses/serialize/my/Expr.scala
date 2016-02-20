package experiments.typeclasses.serialize.my

sealed trait Expression
case class Number(value: Int) extends Expression
case class Plus(lhs: Expression, rhs: Expression) extends Expression
case class Minus(lhs: Expression, rhs: Expression) extends Expression
