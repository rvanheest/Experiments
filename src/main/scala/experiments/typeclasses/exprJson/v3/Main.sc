import experiments.typeclasses.exprJson.v3._

val foo = (1, (2, 3))

JsonWriter.write(foo)
ExpressionEvaluator.evaluate(foo)

implicit def pairMinusExpression[T1: Expr, T2: Expr] = new Expr[(T1, T2)] {
	override def value(pair: (T1, T2)): Int = {
		implicitly[Expr[T1]].value(pair._1) - implicitly[Expr[T2]].value(pair._2)
	}
}

ExpressionEvaluator.evaluate(foo)
