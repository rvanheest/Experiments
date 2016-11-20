package experiments.typelevelProgramming.vector

import experiments.typelevelProgramming.naturalNumbers.{phase1 => Nats}

trait phase1 extends Nats {

  import Nat.+

  sealed trait Vector[Size <: Nat] {
    def ::(head: Int): Vector[NatN[Size]] = NonEmptyVector(head, this)
    def ++[ThatSize <: Nat](that: Vector[ThatSize]): Vector[Size + ThatSize]
    def +(that: Vector[Size]): Vector[Size]
  }
  case object VNil extends Vector[Nat0] {
    override def ++[ThatSize <: Nat](that: Vector[ThatSize]): Vector[ThatSize] = that

    override def +(that: Vector[Nat0]): Vector[Nat0] = this

    override def toString: String = "VNil"
  }
  case class NonEmptyVector[TailSize <: Nat](head: Int, tail: Vector[TailSize]) extends Vector[NatN[TailSize]] {
    type Size = NatN[TailSize]

    override def ++[ThatSize <: Nat](that: Vector[ThatSize]): Vector[NatN[TailSize] + ThatSize] = {
      NonEmptyVector(head, tail ++ that)
    }

    override def +(that: Vector[Size]): Vector[Size] = {
      that match {
        case NonEmptyVector(head2, tail2) => (head + head2) :: (tail + tail2)
      }
    }

    override def toString: String = s"$head :: $tail"
  }
}

object test1 extends App with phase1 {

  val v1 = 1 :: 2 :: VNil
  val v2 = 3 :: VNil
  val v3 = 4 :: 5 :: 6 :: VNil
  val sum = v1 ++ v2 + v3

  println(s"sum: $sum")
}
