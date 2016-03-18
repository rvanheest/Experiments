import experiments.typeclasses.expressionProblem.Expr._
import experiments.typeclasses.expressionProblem.ExprWithNegate._
import experiments.typeclasses.expressionProblem.PrettyPrintExpr._
import experiments.typeclasses.expressionProblem.PrettyPrintExprWithNegate._

val num1 = Number(1)
val num2 = Number(2)
val plus = Plus(num1, num2)

evaluate(num1)
evaluate(num2)
evaluate(plus)

val neg = Negate(plus)
evaluate(neg)

format(num1)
format(num2)
format(plus)
format(neg)
