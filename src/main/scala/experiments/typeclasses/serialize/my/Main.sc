import experiments.typeclasses.serialize.my._
import experiments.typeclasses.serialize.my.Serializers._

import scala.language.reflectiveCalls

val expr1: Expression = Number(5)
val expr2: Expression = Plus(expr1, Number(2))
val expr3: Expression = Minus(expr2, Number(3))
val list = List(expr1, expr2, expr3)

expr1.serialize
expr2.serialize
expr3.serialize
list.serialize
