package monadics.test.instances

import org.scalatest.Inside

import scala.util.{Failure, Try}

class TryTest extends InstanceSpec with Inside {

  property("try combine adds the inner values if both sides are success") {
    import monadics.instances.tryMonad._

    forAll { (x: Int, y: Int) =>
      Try(x) combine Try(y) shouldBe Try(x)
    }
  }

  property("try combine returns the left value if the right value is a failure") {
    import monadics.instances.tryMonad._

    forAll { (x: Int, e: Throwable) =>
      Try(x) combine Try(throw e) shouldBe Try(x)
    }
  }

  property("try combine returns the right value if the left value is a failure") {
    import monadics.instances.tryMonad._

    forAll { (x: Int, e: Throwable) =>
      Try[Int](throw e) combine Try(x) shouldBe Try(x)
    }
  }

  property("try combine returns the success if only one success is in the sequence") {
    import monadics.instances.tryMonad._

    forAll { (x: Int, e1: Throwable, e2: Throwable) =>
      Try[Int](throw e1) combine Try(x) combine Try(throw e2) shouldBe Try(x)
    }
  }

  property("try combine returns the combined successes if some successes are in the sequence") {
    import monadics.instances.tryMonad._

    forAll { (x: Int, y: Int, e: Throwable) =>
      Try(x) combine Try(throw e) combine Try(y) shouldBe Try(x)
    }
  }

  property("try combine returns the combined successes if all values in the sequence are successes") {
    import monadics.instances.tryMonad._

    forAll { (x: Int, y: Int, z: Int) =>
      Try(x) combine Try(z) combine Try(y) shouldBe Try(x)
    }
  }

  property("try combine returns the composed failure when both values were failures") {
    import monadics.instances.tryMonad._

    forAll { (e1: Throwable, e2: Throwable) =>
      inside(Try[Int](throw e1) combine Try(throw e2)) {
        case Failure(e) => e shouldBe e2
      }
    }
  }

  property("try combine returns the composed failure when all values were failures") {
    import monadics.instances.tryMonad._

    forAll { (e1: Throwable, e2: Throwable, e3: Throwable) =>
      inside(Try[Int](throw e1) combine Try(throw e2) combine Try(throw e3)) {
        case Failure(e) => e shouldBe e3
      }
    }
  }

  property("try 'as' replaces the value with another value") {
    import monadics.instances.tryMonad._

    forAll { (x: Int, y: Int) =>
      Try(x).as(y) shouldBe Try(y)
    }
  }

  property("try 'as' does not replace an empty value") {
    import monadics.instances.tryMonad._

    forAll { (x: Int) =>
      val e = new Exception("foo")
      Try(throw e).as(x) shouldBe Failure(e)
    }
  }

  property("try 'void' replaces a value with ()") {
    import monadics.instances.tryMonad._

    forAll { (x: Int) =>
      Try(x).void shouldBe Try(())
    }
  }

  property("try 'void' does not replace an empty value") {
    import monadics.instances.tryMonad._

    val e = new Exception("foo")
    Try(throw e).void shouldBe Failure(e)
  }

  property("try 'zipWith' combines the value with applying the function in a tuple") {
    import monadics.instances.tryMonad._

    forAll { (x: Int, f: Int => String) =>
      Try(x).zipWith(f) shouldBe Try((x, f(x)))
    }
  }

  property("try 'zipWith' does not combines an empty value with a function") {
    import monadics.instances.tryMonad._

    forAll { (f: Int => String) =>
      val e = new Exception("foo")
      Try(throw e).zipWith(f) shouldBe Failure(e)
    }
  }

  property("try 'applicative' should appy the function in the left value to the value on the right") {
    import monadics.instances.tryMonad._

    forAll { (x: Int, f: Int => String) =>
      Try(f) <*> Try(x) shouldBe Try(f(x))
    }
  }

  property("try 'applicative' is empty when the function value is empty") {
    import monadics.instances.tryMonad._

    forAll { (x: Int) =>
      val e = new Exception("foo")
      Try(throw e) <*> Try(x) shouldBe Failure(e)
    }
  }

  property("try 'applicative' is empty when the right value is empty") {
    import monadics.instances.tryMonad._

    forAll { (f: Int => String) =>
      val e = new Exception("foo")
      Try(f) <*> Try(throw e) shouldBe Failure(e)
    }
  }

  property("try 'traverse' should invert the Try and List") {
    import monadics.instances.tryMonad._
    import monadics.instances.list._

    forAll { (x: Int, f: Int => List[String]) =>
      Try(x).traverse(f) shouldBe f(x).map(Try(_))
    }
  }

  property("try 'traverse' should not invert when the value is empty") {
    import monadics.instances.tryMonad._
    import monadics.instances.list._

    forAll { (f: Int => List[String]) =>
      val e = new Exception("foo")
      Try(throw e).traverse(f) shouldBe List(Failure(e))
    }
  }

  property("try 'sequence' should invert the list in an Try value") {
    import monadics.instances.tryMonad._
    import monadics.instances.list._

    forAll { (list: List[Int]) =>
      Try(list).sequence[List, Int] shouldBe list.map(Try(_))
    }
  }

  property("try 'sequence' should not invert an empty option") {
    import monadics.instances.tryMonad._
    import monadics.instances.list._

    val e = new Exception("foo")
    Try[List[Int]](throw e).sequence[List, Int] shouldBe List(Failure(e))
  }
}
