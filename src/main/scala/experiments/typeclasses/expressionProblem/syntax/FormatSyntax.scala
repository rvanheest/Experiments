package experiments.typeclasses.expressionProblem.syntax

import experiments.typeclasses.expressionProblem.prettyprint.PrettyPrint
import experiments.typeclasses.expressionProblem.syntax.FormatSyntax.PrettyPrintOps

import scala.language.implicitConversions

trait FormatSyntax {

  implicit final def syntaxFormat[E: PrettyPrint](expr: E): PrettyPrintOps[E] = new PrettyPrintOps(expr)
}
object FormatSyntax {

  private[FormatSyntax] final class PrettyPrintOps[E: PrettyPrint](expr: E) {
    def format: String = PrettyPrint[E].format(expr)
  }
}
