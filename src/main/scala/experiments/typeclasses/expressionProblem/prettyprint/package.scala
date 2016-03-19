package experiments.typeclasses.expressionProblem

import experiments.typeclasses.expressionProblem.base.Plus

package object prettyprint {

  implicit def addFormat[E: PrettyPrint](expr: E): Object {def format: String} = {
    new {
      def format: String = implicitly[PrettyPrint[E]].format(expr)
    }
  }

  implicit def prettyPrintNumber = new PrettyPrint[base.Number] {
    def format(e: base.Number) = e.value.toString
  }
  implicit def prettyPrintPlus[A: PrettyPrint, B: PrettyPrint] = new PrettyPrint[Plus[A, B]] {
    def format(e: Plus[A, B]) = s"(${implicitly[PrettyPrint[A]].format(e.left)} + ${implicitly[PrettyPrint[B]].format(e.right)})"
  }
}
