import experiments.typeclasses.exprJson.v1._

ExpressionEvaluator.value(Number(5))
ExpressionEvaluator.value(Plus(Number(1), Number(2)))
ExpressionEvaluator.value(Minus(Number(2), Number(1)))

val expr: Expression = Plus(Number(1), Minus(Number(3), Number(2)))
JsonWriter.write(expr)
