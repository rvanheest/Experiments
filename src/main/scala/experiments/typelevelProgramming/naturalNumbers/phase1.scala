package experiments.typelevelProgramming.naturalNumbers

import scala.language.higherKinds

trait phase1 {

  sealed trait Nat {
    type plus[That <: Nat] <: Nat
  }
  object Nat {
    type +[A <: Nat, B <: Nat] = A#plus[B]
  }
  sealed trait Nat0 extends Nat {
    override type plus[That <: Nat] = That
  }
  sealed trait NatN[Prev <: Nat] extends Nat {
    override type plus[That <: Nat] = NatN[Prev#plus[That]]
  }
}

object test1 extends phase1 {

  import Nat.+

  type Nat1 = NatN[Nat0]
  type Nat2 = NatN[Nat1]
  type Nat3 = NatN[Nat2]

  implicitly[Nat0 =:= Nat0]

  implicitly[Nat0 + Nat1 =:= Nat1]
  implicitly[Nat1 + Nat1 =:= Nat2]
  implicitly[Nat1 + Nat2 =:= Nat3]
}
