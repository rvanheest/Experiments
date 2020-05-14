package experiments.typelevelProgramming.booleans

trait phase0 {

  sealed trait BooleanValue {
    def not: BooleanValue
    def or(that: BooleanValue): BooleanValue
  }
  case object TrueValue extends BooleanValue {
    override def not: BooleanValue = FalseValue

    override def or(that: BooleanValue): BooleanValue = TrueValue
  }
  case object FalseValue extends BooleanValue {
    override def not: BooleanValue = TrueValue

    override def or(that: BooleanValue): BooleanValue = that
  }
}

object test0 extends App with phase0 {

  val trueValue = TrueValue
  val falseValue = FalseValue

  println(s"not(true): ${trueValue.not}")
  println(s"not(true): ${falseValue.not}")
  println(s"true or true: ${trueValue.or(trueValue)}")
  println(s"true or false: ${trueValue.or(falseValue)}")
  println(s"false or true: ${falseValue.or(trueValue)}")
  println(s"false or false: ${falseValue.or(falseValue)}")
}
