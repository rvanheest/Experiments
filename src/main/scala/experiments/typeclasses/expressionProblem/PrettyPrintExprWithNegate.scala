package experiments.typeclasses.expressionProblem

import experiments.typeclasses.expressionProblem.ExprWithNegate.Negate
import experiments.typeclasses.expressionProblem.PrettyPrintExpr.PrettyPrint

object PrettyPrintExprWithNegate {

  implicit def prettyPrintNegate[E: PrettyPrint] = new PrettyPrint[Negate[E]] {
    def format(e: Negate[E]) = s"-${implicitly[PrettyPrint[E]].format(e.expr)}"
  }
}
