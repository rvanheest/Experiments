package experiments.typeclasses.expressionProblem.base

trait Expr[E] {
  def eval(e: E): Int
}
