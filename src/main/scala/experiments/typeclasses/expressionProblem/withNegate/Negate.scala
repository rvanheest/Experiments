package experiments.typeclasses.expressionProblem.withNegate

import experiments.typeclasses.expressionProblem.base.Expr

case class Negate[E: Expr](expr: E)
