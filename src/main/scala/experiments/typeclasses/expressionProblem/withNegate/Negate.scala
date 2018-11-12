package experiments.typeclasses.expressionProblem.withNegate

import experiments.typeclasses.expressionProblem.base.Expr

case class Negate[E: Expr](expr: E)
object Negate {
  implicit def evalNegate[E: Expr]: Expr[Negate[E]] = Expr(e => -Expr[E].eval(e.expr))
}
