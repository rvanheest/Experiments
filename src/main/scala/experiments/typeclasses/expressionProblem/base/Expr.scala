package experiments.typeclasses.expressionProblem.base

trait Expr[E] {
  def eval(e: E): Int
}
object Expr {
  def apply[E](implicit E: Expr[E]): Expr[E] = E

  def from[E](f: E => Int): Expr[E] = f(_)
}
