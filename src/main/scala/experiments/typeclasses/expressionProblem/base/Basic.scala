package experiments.typeclasses.expressionProblem.base

case class Number(value: Int)
case class Plus[A: Expr, B: Expr](left: A, right: B)
