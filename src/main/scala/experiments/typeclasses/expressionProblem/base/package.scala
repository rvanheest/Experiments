package experiments.typeclasses.expressionProblem

import scala.language.implicitConversions

package object base {
  implicit def addEvaluate[E: Expr](expr: E): Object {def evaluate: Int} = {
    new {
      def evaluate: Int = implicitly[Expr[E]].eval(expr)
    }
  }

  implicit def evalNumber = new Expr[Number] {
    def eval(e: Number): Int = e.value
  }
  implicit def evalPlus[A: Expr, B: Expr] = new Expr[Plus[A, B]] {
    def eval(e: Plus[A, B]): Int = implicitly[Expr[A]].eval(e.left) + implicitly[Expr[B]].eval(e.right)
  }
}
