package experiments.typelevelProgramming.vector

trait phase0 {

  sealed trait Vector {
    def ::(head: Int): Vector = NonEmptyVector(head, this)
    def +(that: Vector): Vector
    def size: Int
  }
  case object VNil extends Vector {
    override def +(that: Vector): Vector = {
      require(that == VNil)
      this
    }

    override def size: Int = 0

    override def toString: String = "VNil"
  }
  case class NonEmptyVector(head: Int, tail: Vector) extends Vector {
    def +(that: Vector): Vector = {
      require(that.size == size)
      that match {
        case VNil => throw new IllegalArgumentException("unequal size")
        case NonEmptyVector(h, t) => (head + h) :: (tail + t)
      }
    }

    override def size: Int = 1 + tail.size

    override def toString: String = s"$head :: $tail"
  }
}

object test0 extends App with phase0 {

  val empty = VNil
  val s1 = 1 :: VNil
  val s2 = 1 :: 3 :: VNil
  val s3 = 1 :: 3 :: 5 :: VNil

  val s4 = s3 + s3

  println(s"empty.size = ${empty.size}")
  println(s"s1.size = ${s1.size}")
  println(s"s2.size = ${s2.size}")
  println(s"s3.size = ${s3.size}")
  println(s"s4.size = ${s4.size}")
  println(s"s4 = $s4")
}
