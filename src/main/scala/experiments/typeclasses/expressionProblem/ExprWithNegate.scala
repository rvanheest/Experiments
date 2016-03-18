package experiments.typeclasses.expressionProblem

import experiments.typeclasses.expressionProblem.Expr.Expr

object ExprWithNegate {

  case class Negate[E: Expr](expr: E)

  implicit def evalNegate[E: Expr] = new Expr[Negate[E]] {
    def eval(e: Negate[E]) = -implicitly[Expr[E]].eval(e.expr)
  }
}
