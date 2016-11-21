package experiments.typelevelProgramming.booleans

import scala.language.higherKinds

trait phase1 {

  sealed trait BooleanType {
    type Not <: BooleanType
    type Or[That <: BooleanType] <: BooleanType
    type And[That <: BooleanType] <: BooleanType
    type Then[That <: BooleanType] <: BooleanType
  }
  object BooleanType {
    type \/[A <: BooleanType, B <: BooleanType] = A#Or[B]
    type /\[A <: BooleanType, B <: BooleanType] = A#And[B]
    type ==>[A <: BooleanType, B <: BooleanType] = A#Then[B]
  }
  sealed trait TrueType extends BooleanType {
    override type Not = FalseType
    override type Or[That <: BooleanType] = TrueType
    override type And[That <: BooleanType] = That
    override type Then[That <: BooleanType] = That
  }
  sealed trait FalseType extends BooleanType {
    override type Not = TrueType
    override type Or[That <: BooleanType] = That
    override type And[That <: BooleanType] = FalseType
    override type Then[That <: BooleanType] = TrueType
  }
}

// compile to test
object test1 extends phase1 {

  import BooleanType.{\/, /\, ==>}

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

  // and
  implicitly[TrueType#And[TrueType] =:= TrueType]
  implicitly[TrueType#And[FalseType] =:= FalseType]
  implicitly[FalseType#And[TrueType] =:= FalseType]
  implicitly[FalseType#And[FalseType] =:= FalseType]

  // /\
  implicitly[TrueType /\ TrueType =:= TrueType]
  implicitly[TrueType /\ FalseType =:= FalseType]
  implicitly[FalseType /\ TrueType =:= FalseType]
  implicitly[FalseType /\ FalseType =:= FalseType]

  // then
  implicitly[TrueType#Then[TrueType] =:= TrueType]
  implicitly[TrueType#Then[FalseType] =:= FalseType]
  implicitly[FalseType#Then[TrueType] =:= TrueType]
  implicitly[FalseType#Then[TrueType] =:= TrueType]

  // ==>
  implicitly[TrueType ==> TrueType =:= TrueType]
  implicitly[TrueType ==> FalseType =:= FalseType]
  implicitly[FalseType ==> TrueType =:= TrueType]
  implicitly[FalseType ==> FalseType =:= TrueType]


  // distributivity of \/ over /\
  implicitly[(TrueType \/ (TrueType /\ TrueType)) =:= ((TrueType \/ TrueType) /\ (TrueType \/ TrueType))]
  implicitly[(TrueType \/ (TrueType /\ FalseType)) =:= ((TrueType \/ TrueType) /\ (TrueType \/ FalseType))]
  implicitly[(TrueType \/ (FalseType /\ TrueType)) =:= ((TrueType \/ FalseType) /\ (TrueType \/ TrueType))]
  implicitly[(TrueType \/ (FalseType /\ FalseType)) =:= ((TrueType \/ FalseType) /\ (TrueType \/ FalseType))]
  implicitly[(FalseType \/ (TrueType /\ TrueType)) =:= ((FalseType \/ TrueType) /\ (FalseType \/ TrueType))]
  implicitly[(FalseType \/ (TrueType /\ FalseType)) =:= ((FalseType \/ TrueType) /\ (FalseType \/ FalseType))]
  implicitly[(FalseType \/ (FalseType /\ TrueType)) =:= ((FalseType \/ FalseType) /\ (FalseType \/ TrueType))]
  implicitly[(FalseType \/ (FalseType /\ FalseType)) =:= ((FalseType \/ FalseType) /\ (FalseType \/ FalseType))]

  // double negation
  implicitly[(TrueType#Not)#Not =:= TrueType]
  implicitly[(FalseType#Not)#Not =:= FalseType]

  // De Morgan 1
  implicitly[TrueType#Not /\ TrueType#Not =:= (TrueType \/ TrueType)#Not]
  implicitly[TrueType#Not /\ FalseType#Not =:= (TrueType \/ FalseType)#Not]
  implicitly[FalseType#Not /\ TrueType#Not =:= (FalseType \/ TrueType)#Not]
  implicitly[FalseType#Not /\ FalseType#Not =:= (FalseType \/ FalseType)#Not]

  // De Morgan 2
  implicitly[TrueType#Not \/ TrueType#Not =:= (TrueType /\ TrueType)#Not]
  implicitly[TrueType#Not \/ FalseType#Not =:= (TrueType /\ FalseType)#Not]
  implicitly[FalseType#Not \/ TrueType#Not =:= (FalseType /\ TrueType)#Not]
  implicitly[FalseType#Not \/ FalseType#Not =:= (FalseType /\ FalseType)#Not]
}
