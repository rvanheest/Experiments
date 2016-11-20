package experiments.typelevelProgramming.vector

import experiments.typelevelProgramming.naturalNumbers.{phase1 => Nats}

import scala.language.{higherKinds, reflectiveCalls}

trait phase2 extends Nats {

  import Nat.+

  sealed trait TypeList {
    type size <: Nat
    type reduce <: Nat
    type map[F[Nat] <: Nat] <: TypeList
    type fold[A <: Nat, F[_ <: Nat, _ <: Nat] <: Nat] <: Nat
  }
  sealed trait TNil extends TypeList {
    override type size = Nat0
    override type reduce = Nat0
    override type map[F[Nat] <: Nat] = TNil
    override type fold[A <: Nat, F[_ <: Nat, _ <: Nat] <: Nat] = A
  }
  sealed trait ::[H <: Nat, T <: TypeList] extends TypeList {
    override type size = NatN[T#size]
    override type reduce = H + (T#reduce)
    override type map[F[Nat] <: Nat] = F[H] :: T#map[F]
    override type fold[A <: Nat, F[_ <: Nat, _ <: Nat] <: Nat] = F[H, T#fold[A, F]]
  }
}

object test2 extends phase2 {

  import Nat.+

  type Nat1 = NatN[Nat0]
  type Nat2 = NatN[Nat1]
  type Nat3 = NatN[Nat2]
  type Nat4 = NatN[Nat3]
  type Nat5 = NatN[Nat4]
  type Nat6 = NatN[Nat5]

  type L = Nat1 :: Nat2 :: Nat3 :: TNil
  implicitly[L#size =:= Nat3]
  implicitly[L#reduce =:= Nat6]

  type L1 = Nat0 :: Nat1 :: Nat2 :: TNil
  type L2 = Nat1 :: Nat2 :: Nat3 :: TNil

  implicitly[L1#map[({type F[i <: Nat] = NatN[i]})#F] =:= L2]

  type L3 = Nat2 :: Nat1 :: Nat0 :: Nat1 :: TNil
  implicitly[L3#fold[Nat0, ({type F[A <: Nat, B <: Nat] = A + B})#F] =:= Nat4]
}
