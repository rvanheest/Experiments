import experiments.typeclasses.exprJson.v2._

ExpressionEvaluator.value(Number(5))
ExpressionEvaluator.value(Plus(Number(1), Number(2)))
ExpressionEvaluator.value(Minus(Number(2), Number(1)))

val expr: Expression = Plus(Number(1), Minus(Number(3), Number(2)))
implicit val converter = new JsonConverter[Expression] {
	override def convertToJson(expr: Expression): JsonValue = {
		expr match {
			case Number(value) => JsonNumber(value)
			case Plus(lhs, rhs) => JsonObject(
				Map("op" -> JsonString("+"),
					"lhs" -> convertToJson(lhs),
					"rhs" -> convertToJson(rhs)))
			case Minus(lhs, rhs) => JsonObject(
				Map("op" -> JsonString("-"),
					"lhs" -> convertToJson(lhs),
					"rhs" -> convertToJson(rhs)))
		}
	}
}

JsonWriter.write(expr)
