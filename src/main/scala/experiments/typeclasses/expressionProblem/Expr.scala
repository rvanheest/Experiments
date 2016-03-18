package experiments.typeclasses.expressionProblem

object Expr {

  trait Expr[E] {
    def eval(e: E): Int
  }
  implicit def addEvaluate[E: Expr](expr: E): Object {def evaluate: Int} = {
    new {
      def evaluate: Int = implicitly[Expr[E]].eval(expr)
    }
  }

  case class Number(value: Int)
  case class Plus[A: Expr, B: Expr](left: A, right: B)

  implicit def evalNumber = new Expr[Number] {
    def eval(e: Number) = e.value
  }
  implicit def evalPlus[A: Expr, B: Expr] = new Expr[Plus[A, B]] {
    def eval(e: Plus[A, B]) = implicitly[Expr[A]].eval(e.left) + implicitly[Expr[B]].eval(e.right)
  }
}
