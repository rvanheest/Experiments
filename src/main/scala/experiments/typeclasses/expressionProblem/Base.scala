package experiments.typeclasses.expressionProblem

object Expr {

  trait Expr[E] {
    def eval(e: E): Int
  }
  def evaluate[A : Expr](expr: A) = implicitly[Expr[A]].eval(expr)

  case class Number(value: Int)
  case class Plus[A : Expr, B : Expr](left: A, right: B)

  implicit def evalNumber = new Expr[Number] {
    def eval(e: Number) = e.value
  }
  implicit def evalPlus[A : Expr, B : Expr] = new Expr[Plus[A, B]] {
    def eval(e: Plus[A, B]) = implicitly[Expr[A]].eval(e.left) + implicitly[Expr[B]].eval(e.right)
  }
}

object ExprWithNegate {
  import experiments.typeclasses.expressionProblem.Expr.Expr

  case class Negate[E: Expr](expr: E)

  implicit def evalNegate[E: Expr] = new Expr[Negate[E]] {
    def eval(e: Negate[E]) = - implicitly[Expr[E]].eval(e.expr)
  }
}

object PrettyPrintExpr {
  import experiments.typeclasses.expressionProblem.Expr.{Number, Plus}

  trait PrettyPrint[E] {
    def format(e: E): String
  }
  def format[E: PrettyPrint](expr: E) = implicitly[PrettyPrint[E]].format(expr)

  implicit def prettyPrintNumber = new PrettyPrint[Number] {
    def format(e: Number) = e.value.toString
  }
  implicit def prettyPrintPlus[A: PrettyPrint, B: PrettyPrint] = new PrettyPrint[Plus[A, B]] {
    def format(e: Plus[A, B]) = s"(${implicitly[PrettyPrint[A]].format(e.left)} + ${implicitly[PrettyPrint[B]].format(e.right)})"
  }
}

object PrettyPrintExprWithNegate {
  import experiments.typeclasses.expressionProblem.ExprWithNegate.Negate
  import experiments.typeclasses.expressionProblem.PrettyPrintExpr.PrettyPrint

  implicit def prettyPrintNegate[E: PrettyPrint] = new PrettyPrint[Negate[E]] {
    def format(e: Negate[E]) = s"-${implicitly[PrettyPrint[E]].format(e.expr)}"
  }
}
