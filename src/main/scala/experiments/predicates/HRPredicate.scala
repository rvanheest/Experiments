package experiments.predicates

// TODO Result is a Monoid!
sealed abstract class Result {
  def isSuccess: Boolean
  def isFail: Boolean
  def or(that: => Result): Result
  def and(that: => Result): Result
  def unary_! : Result
}
case object Success extends Result {
  def isSuccess = true
  def isFail = false
  def or(that: => Result): Result = this
  def and(that: => Result): Result = that
  def unary_! : Result = Fail("???") // TODO what to return here?
}
case class Fail(reason: String) extends Result {
  def isSuccess = false
  def isFail = true
  def or(that: => Result): Result = that
  def and(that: => Result): Result = this
  def unary_! : Result = Success
}

case class HRPredicate[T](name: String, predicate: T => Result) {

  def apply(t: T): Boolean = predicate(t).isSuccess

  def test(t: T): Result = {
    predicate(t) match {
      case Success => Success
      case Fail(reason) => Fail(s"$name failed: $reason")
    }
  }

  def lift(opName: String, op: (Result, Result) => Result)(that: => HRPredicate[T]): HRPredicate[T] = {
    HRPredicate(s"($name $opName ${that.name})", t => op(predicate(t), that.predicate(t)))
  }

  def unary_! : HRPredicate[T] = {
    HRPredicate(s"not $name", t => !predicate(t))
  }

  def or(that: => HRPredicate[T]): HRPredicate[T] = {
    lift("or", _ or _)(that)
  }

  def and(that: => HRPredicate[T]): HRPredicate[T] = {
    lift("and", _ and _)(that)
  }
}

object test extends App {

  val s = Success
  val f1 = Fail("f1")
  val f2 = Fail("f2")

  println(s or s)
  println(s or f1)
  println(f1 or s)
  println(f1 or f2)

  println(s and s)
  println(s and f1)
  println(f1 and s)
  println(f1 and f2)

  val p1 = HRPredicate[Int]("p1", i => if (i % 2 == 0) Success else Fail("the integer wasn't even"))
  val p2 = HRPredicate[Int]("p2", i => if (i > 0) Success else Fail("the integer wasn't strictly larger than 0"))
  val p3 = p1 or p2
  val p4 = p1 and p2
  val p5 = !p4
  val p6 = !p3

  println(p1.test(5))

  println(p3.test(5))
  println(p3.test(6))
  println(p3.test(-1))

  println(p4 test 4)
  println(p4 test 5)
  println(p4 test -2)

  println(p5.test(5))
  println(p5.test(6))
  println(p5.test(-1))

  println(p6.test(5))
  println(p6.test(6))
  println(p6.test(-1))
}
