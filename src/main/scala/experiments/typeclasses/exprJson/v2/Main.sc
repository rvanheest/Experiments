package experiments.scala.typeclasses.exprJson.v2

import experiments.scala.typeclasses.exprJson.v2._

object Main {
	ExpressionEvaluator.value(Number(5))      //> res0: Int = 5
	ExpressionEvaluator.value(Plus(Number(1), Number(2)))
                                                  //> res1: Int = 3
	ExpressionEvaluator.value(Minus(Number(2), Number(1)))
                                                  //> res2: Int = 1

	val expr: Expression = Plus(Number(1), Minus(Number(3), Number(2)))
                                                  //> expr  : experiments.scala.typeclasses.exprJson.v2.Expression = Plus(Number(1
                                                  //| ),Minus(Number(3),Number(2)))
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
	}                                         //> converter  : experiments.scala.typeclasses.exprJson.v2.JsonConverter[experim
                                                  //| ents.scala.typeclasses.exprJson.v2.Expression] = experiments.scala.typeclass
                                                  //| es.exprJson.v2.Main$$anonfun$main$1$$anon$1@30c7da1e
	JsonWriter.write(expr)
}