package experiments.typelevelProgramming.naturalNumbers

trait phase0 {

  sealed trait Nat {
    def +(that: Nat): Nat
  }
  case object Nat0 extends Nat {
    override def +(that: Nat): Nat = that
  }
  case class NatN(prev: Nat) extends Nat {
    override def +(that: Nat): Nat = NatN(prev + that)
  }
}

object test0 extends App with phase0 {
  val nat0 = Nat0
  val nat1 = NatN(nat0)
  val nat2 = NatN(nat1)
  val nat3 = NatN(nat2)

  println(s"0 + 1 = ${nat0 + nat1}")
  println(s"1 + 1 = ${nat1 + nat1}")
  println(s"1 + 2 = ${nat1 + nat2}")
}
