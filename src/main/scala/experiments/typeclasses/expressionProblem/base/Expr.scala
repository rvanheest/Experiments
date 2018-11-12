package experiments.typeclasses.expressionProblem.base

trait Expr[E] {
  def eval(e: E): Int
}
object Expr {
  def apply[E](implicit E: Expr[E]): Expr[E] = E

  def apply[E](f: E => Int): Expr[E] = new Expr[E] {
    def eval(e: E): Int = f(e)
  }

  implicit class ExprSyntax[E: Expr](val expr: E) extends AnyVal {
    def evaluate: Int = Expr[E].eval(expr)
  }
}
