package experiments.typeclasses.exprJson.v2

sealed trait Expression
case class Number(value: Int) extends Expression
case class Plus(lhs: Expression, rhs: Expression) extends Expression
case class Minus(lhs: Expression, rhs: Expression) extends Expression

object ExpressionEvaluator {
	def value(expression: Expression): Int = expression match {
		case Number(value) => value
		case Plus(lhs, rhs) => value(lhs) + value(rhs)
		case Minus(lhs, rhs) => value(lhs) - value(rhs)
	}
}
