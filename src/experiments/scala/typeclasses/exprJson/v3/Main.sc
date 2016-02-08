package experiments.scala.typeclasses.exprJson.v3

import experiments.scala.typeclasses.exprJson.v3._

object Main3 {
	val foo = (1, (2, 3))                     //> foo  : (Int, (Int, Int)) = (1,(2,3))

	JsonWriter.write(foo)                     //> res0: String = { fst: 1, snd: { fst: 2, snd: 3 } }
	ExpressionEvaluator.evaluate(foo)         //> res1: Int = 6

	implicit def pairMinusExpression[T1: Expr, T2: Expr] = new Expr[(T1, T2)] {
		override def value(pair: (T1, T2)): Int = {
			implicitly[Expr[T1]].value(pair._1) - implicitly[Expr[T2]].value(pair._2)
		}
	}                                         //> pairMinusExpression: [T1, T2](implicit evidence$3: experiments.scala.typecla
                                                  //| sses.exprJson.v3.Expr[T1], implicit evidence$4: experiments.scala.typeclasse
                                                  //| s.exprJson.v3.Expr[T2])experiments.scala.typeclasses.exprJson.v3.Expr[(T1, T
                                                  //| 2)]

	ExpressionEvaluator.evaluate(foo)         //> res2: Int = 2
}