package experiments.typeclasses.expressionProblem

import experiments.typeclasses.expressionProblem.Expr.{Number, Plus}

object PrettyPrintExpr {

  trait PrettyPrint[E] {
    def format(e: E): String
  }
  implicit def addFormat[E: PrettyPrint](expr: E): Object {def format: String} = {
    new {
      def format: String = implicitly[PrettyPrint[E]].format(expr)
    }
  }

  implicit def prettyPrintNumber = new PrettyPrint[Number] {
    def format(e: Number) = e.value.toString
  }
  implicit def prettyPrintPlus[A: PrettyPrint, B: PrettyPrint] = new PrettyPrint[Plus[A, B]] {
    def format(e: Plus[A, B]) = s"(${implicitly[PrettyPrint[A]].format(e.left)} + ${implicitly[PrettyPrint[B]].format(e.right)})"
  }
}
