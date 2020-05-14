package experiments.typeclasses.expressionProblem.syntax

import experiments.typeclasses.expressionProblem.base.Expr
import experiments.typeclasses.expressionProblem.syntax.ExprSyntax.ExprOps

import scala.language.implicitConversions

trait ExprSyntax {

  implicit final def syntaxEvaluate[E: Expr](e: E): ExprOps[E] = new ExprOps(e)
}

object ExprSyntax {

  private[ExprSyntax] final class ExprOps[E: Expr](e: E) {
    def evaluate: Int = Expr[E].eval(e)
  }
}
