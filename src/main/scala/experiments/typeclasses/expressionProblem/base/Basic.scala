package experiments.typeclasses.expressionProblem.base

case class Number(value: Int)
object Number {
  implicit val evalNumber: Expr[Number] = Expr[Number](_.value)
}

case class Plus[A: Expr, B: Expr](left: A, right: B)
object Plus {
  implicit def evalPlus[A: Expr, B: Expr]: Expr[Plus[A, B]] = {
    Expr[Plus[A, B]](e => Expr[A].eval(e.left) + Expr[B].eval(e.right))
  }
}
