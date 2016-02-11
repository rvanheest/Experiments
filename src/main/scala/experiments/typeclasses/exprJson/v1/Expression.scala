package experiments.scala.typeclasses.exprJson.v1

sealed trait Expression extends JsonConvertible
case class Number(value: Int) extends Expression {
	def convertToJson: JsonValue = JsonNumber(value)
}
case class Plus(lhs: Expression, rhs: Expression) extends Expression {
	def convertToJson: JsonValue = JsonObject(
		Map("op" -> JsonString("+"),
			"lhs" -> lhs.convertToJson,
			"rhs" -> rhs.convertToJson))
}
case class Minus(lhs: Expression, rhs: Expression) extends Expression {
	def convertToJson: JsonValue = JsonObject(
		Map("op" -> JsonString("-"),
			"lhs" -> lhs.convertToJson,
			"rhs" -> rhs.convertToJson))
}

object ExpressionEvaluator {
	def value(expression: Expression): Int = expression match {
		case Number(value) => value
		case Plus(lhs, rhs) => value(lhs) + value(rhs)
		case Minus(lhs, rhs) => value(lhs) - value(rhs)
	}
}
