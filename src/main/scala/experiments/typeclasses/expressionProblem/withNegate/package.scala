package experiments.typeclasses.expressionProblem

import experiments.typeclasses.expressionProblem.base.Expr

package object withNegate {
  implicit def evalNegate[E: Expr] = new Expr[Negate[E]] {
    def eval(e: Negate[E]): Int = -implicitly[Expr[E]].eval(e.expr)
  }
}
