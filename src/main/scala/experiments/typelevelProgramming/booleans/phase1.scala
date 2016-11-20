package experiments.typelevelProgramming.booleans

import scala.language.higherKinds

trait phase1 {

  sealed trait BooleanType {
    type Not <: BooleanType
    type Or[That <: BooleanType] <: BooleanType
  }
  object BooleanType {
    type \/[A <: BooleanType, B <: BooleanType] = A#Or[B]
  }
  sealed trait TrueType extends BooleanType {
    override type Not = FalseType
    override type Or[That <: BooleanType] = TrueType
  }
  sealed trait FalseType extends BooleanType {
    override type Not = TrueType
    override type Or[That <: BooleanType] = That
  }
}

// compile to test
object test1 extends phase1 {

  import BooleanType.\/

  // identity
  implicitly[TrueType =:= TrueType]
  implicitly[FalseType =:= FalseType]

  // not
  implicitly[TrueType#Not =:= FalseType]
  implicitly[FalseType#Not =:= TrueType]

  // or
  implicitly[TrueType#Or[TrueType] =:= TrueType]
  implicitly[TrueType#Or[FalseType] =:= TrueType]
  implicitly[FalseType#Or[TrueType] =:= TrueType]
  implicitly[FalseType#Or[FalseType] =:= FalseType]

  // \/
  implicitly[TrueType \/ TrueType =:= TrueType]
  implicitly[TrueType \/ FalseType =:= TrueType]
  implicitly[FalseType \/ TrueType =:= TrueType]
  implicitly[FalseType \/ FalseType =:= FalseType]
}
