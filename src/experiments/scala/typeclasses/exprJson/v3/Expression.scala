package experiments.scala.typeclasses.exprJson.v3

sealed trait Expression
case class Number(value: Int) extends Expression
case class Plus(lhs: Expression, rhs: Expression) extends Expression
case class Minus(lhs: Expression, rhs: Expression) extends Expression

trait Expr[A] {
	def value(expr: A): Int
}
object Expr {
	implicit val intExpression = new Expr[Int] {
		override def value(n: Int): Int = n
	}

	implicit def pairPlusExpression[T1: Expr, T2: Expr] = new Expr[(T1, T2)] {
		override def value(pair: (T1, T2)): Int = implicitly[Expr[T1]].value(pair._1) + implicitly[Expr[T2]].value(pair._2)
	}
}

object ExpressionEvaluator {
	def value(expression: Expression): Int = expression match {
		case Number(value) => value
		case Plus(lhs, rhs) => value(lhs) + value(rhs)
		case Minus(lhs, rhs) => value(lhs) - value(rhs)
	}
	def evaluate[A: Expr](expr: A): Int = implicitly[Expr[A]].value(expr)
}
