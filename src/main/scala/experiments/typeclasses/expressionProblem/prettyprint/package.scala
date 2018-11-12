package experiments.typeclasses.expressionProblem

import experiments.typeclasses.expressionProblem.base.Plus

import scala.language.implicitConversions

package object prettyprint {

  implicit val prettyPrintNumber: PrettyPrint[base.Number] = PrettyPrint(_.value.toString)

  implicit def prettyPrintPlus[A: PrettyPrint, B: PrettyPrint]: PrettyPrint[Plus[A, B]] = {
    PrettyPrint(e => s"(${ PrettyPrint[A].format(e.left) } + ${ PrettyPrint[B].format(e.right) })")
  }
}
