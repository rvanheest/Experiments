import experiments.typeclasses.expressionProblem.base.{Number, Plus}
import experiments.typeclasses.expressionProblem.withNegate.Negate
import experiments.typeclasses.expressionProblem.prettyprint._
import experiments.typeclasses.expressionProblem.withNegatePrettyPrint._

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
