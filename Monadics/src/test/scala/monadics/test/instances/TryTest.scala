package monadics.test.instances

import scala.util.{Failure, Try}

class TryTest extends InstanceSpec {

  property("try 'as' replaces the value with another value") {
    import monadics.instances.tryMonad._

    forAll { (x: Int, y: Int) =>
      Try(x).as(y) shouldBe Try(y)
    }
  }

  property("try 'as' does not replace an empty value") {
    import monadics.instances.tryMonad._

    forAll { (x: Int, e: Exception) =>
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

    forAll { (e: Exception) =>
      Try(throw e).void shouldBe Failure(e)
    }
  }

  property("try 'zipWith' combines the value with applying the function in a tuple") {
    import monadics.instances.tryMonad._

    forAll { (x: Int, f: Int => String) =>
      Try(x).zipWith(f) shouldBe Try((x, f(x)))
    }
  }

  property("try 'zipWith' does not combines an empty value with a function") {
    import monadics.instances.tryMonad._

    forAll { (f: Int => String, e: Exception) =>
      Try(throw e).zipWith(f) shouldBe Failure(e)
    }
  }

  property("try 'applicative' should apply the function in the left value to the value on the right") {
    import monadics.instances.tryMonad._

    forAll { (x: Int, f: Int => String) =>
      Try(f) <*> Try(x) shouldBe Try(f(x))
    }
  }

  property("try 'applicative' is empty when the function value is empty") {
    import monadics.instances.tryMonad._

    forAll { (x: Int, e: Exception) =>
      Try(throw e) <*> Try(x) shouldBe Failure(e)
    }
  }

  property("try 'applicative' is empty when the right value is empty") {
    import monadics.instances.tryMonad._

    forAll { (f: Int => String, e: Exception) =>
      Try(f) <*> Try(throw e) shouldBe Failure(e)
    }
  }

  property("try 'andThen' replaces the value with the other value") {
    import monadics.instances.tryMonad._

    forAll { (a: Int, b: Int) =>
      Try(a).andThen(Try(b)) shouldBe Try(b)
    }
  }

  property("try 'andThen' does not replace the value when the original is empty") {
    import monadics.instances.tryMonad._

    forAll { (a: Int, e: Exception) =>
      Try(throw e).andThen(Try(a)) shouldBe Failure(e)
    }
  }

  property("try 'andThen' replaces the value with an empty if the new value is empty") {
    import monadics.instances.tryMonad._

    forAll { (a: Int, e: Exception) =>
      Try(a).andThen(Try(throw e)) shouldBe Failure(e)
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

    forAll { (f: Int => List[String], e: Exception) =>
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

    forAll { (e: Exception) =>
      Try[List[Int]](throw e).sequence[List, Int] shouldBe List(Failure(e))
    }
  }
}
