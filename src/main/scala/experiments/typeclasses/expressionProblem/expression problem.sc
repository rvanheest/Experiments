import experiments.typeclasses.expressionProblem.Expr._
import experiments.typeclasses.expressionProblem.ExprWithNegate._
import experiments.typeclasses.expressionProblem.PrettyPrintExpr._
import experiments.typeclasses.expressionProblem.PrettyPrintExprWithNegate._

import scala.language.reflectiveCalls

val num1 = Number(1)
val num2 = Number(2)
val plus = Plus(num1, num2)

num1.evaluate
num2.evaluate
plus.evaluate

val neg = Negate(plus)
neg.evaluate

num1.format
num2.format
plus.format
neg.format
