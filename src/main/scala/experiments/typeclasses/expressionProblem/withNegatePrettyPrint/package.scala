package experiments.typeclasses.expressionProblem

import experiments.typeclasses.expressionProblem.prettyprint.PrettyPrint
import experiments.typeclasses.expressionProblem.withNegate.Negate

package object withNegatePrettyPrint {
  implicit def prettyPrintNegate[E: PrettyPrint] = new PrettyPrint[Negate[E]] {
    def format(e: Negate[E]): String = s"-${implicitly[PrettyPrint[E]].format(e.expr)}"
  }
}
