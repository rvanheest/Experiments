package experiments.scala.typeclasses.exprJson.v1

import experiments.scala.typeclasses.exprJson.v1._

object Main {
	ExpressionEvaluator.value(Number(5))      //> res0: Int = 5
	ExpressionEvaluator.value(Plus(Number(1), Number(2)))
                                                  //> res1: Int = 3
	ExpressionEvaluator.value(Minus(Number(2), Number(1)))
                                                  //> res2: Int = 1
	
	val expr: Expression = Plus(Number(1), Minus(Number(3), Number(2)))
                                                  //> expr  : experiments.scala.typeclasses.exprJson.v1.Expression = Plus(Number(1
                                                  //| ),Minus(Number(3),Number(2)))
	JsonWriter.write(expr)                    //> res3: String = { op: "+", lhs: 1, rhs: { op: "-", lhs: 3, rhs: 2 } }
}
	