package experiments.typeclasses.expressionProblem

import experiments.typeclasses.expressionProblem.prettyprint.PrettyPrint
import experiments.typeclasses.expressionProblem.withNegate.Negate

package object withNegatePrettyPrint {
  implicit def prettyPrintNegate[E: PrettyPrint]: PrettyPrint[Negate[E]] = {
    PrettyPrint.from(e => s"-${PrettyPrint[E].format(e.expr)}")
  }
}
