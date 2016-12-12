package monadics.test.instances

class OptionSpec extends InstanceSpec {

  property("option 'combine' adds the inner values if both sides are not empty") {
    import monadics.instances.option._
    import monadics.instances.monoids.values._

    forAll { (x: Int, y: Int) =>
      Option(x) combine Option(y) shouldBe Option(x + y)
    }
  }

  property("option 'combine' returns the left value if the right value is empty") {
    import monadics.instances.option._
    import monadics.instances.monoids.values._

    forAll { (x: Int) =>
      Option(x) combine Option.empty shouldBe Option(x)
    }
  }

  property("option 'combine' returns the right value if the left value is empty") {
    import monadics.instances.option._
    import monadics.instances.monoids.values._

    forAll { (x: Int) =>
      Option.empty[Int] combine Option(x) shouldBe Option(x)
    }
  }

  property("option 'combine' returns an empty value if both values are empty") {
    import monadics.instances.option._
    import monadics.instances.monoids.values._

    Option.empty[Int] combine Option.empty[Int] shouldBe Option.empty[Int]
  }

  property("option 'as' replaces the value with another value") {
    import monadics.instances.option._

    forAll { (x: Int, y: Int) =>
      Option(x).as(y) shouldBe Option(y)
    }
  }

  property("option 'as' does not replace an empty value") {
    import monadics.instances.option._

    forAll { (x: Int) =>
      Option.empty[Int].as(x) shouldBe Option.empty[Int]
    }
  }

  property("option 'void' replaces a value with ()") {
    import monadics.instances.option._

    forAll { (x: Int) =>
      Option(x).void shouldBe Option(())
    }
  }

  property("option 'void' does not replace an empty value") {
    import monadics.instances.option._

    Option.empty.void shouldBe Option.empty
  }

  property("option 'zipWith' combines the value with applying the function in a tuple") {
    import monadics.instances.option._

    forAll { (x: Int, f: Int => String) =>
      Option(x).zipWith(f) shouldBe Option((x, f(x)))
    }
  }

  property("option 'zipWith' does not combines an empty value with a function") {
    import monadics.instances.option._

    forAll { (f: Int => String) =>
      Option.empty[Int].zipWith(f) shouldBe Option.empty[Int]
    }
  }

  property("option 'applicative' should apply the function in the left value to the value on the right") {
    import monadics.instances.option._

    forAll { (x: Int, f: Int => String) =>
      Option(f) <*> Option(x) shouldBe Option(f(x))
    }
  }

  property("option 'applicative' is empty when the function value is empty") {
    import monadics.instances.option._

    forAll { (x: Int) =>
      Option.empty[Int => String] <*> Option(x) shouldBe Option.empty[String]
    }
  }

  property("option 'applicative' is empty when the right value is empty") {
    import monadics.instances.option._

    forAll { (f: Int => String) =>
      Option(f) <*> Option.empty[Int] shouldBe Option.empty[String]
    }
  }

  property("option 'andThen' replaces the value with the other value") {
    import monadics.instances.option._

    forAll { (a: Int, b: Int) =>
      Option(a).andThen(Option(b)) shouldBe Option(b)
    }
  }

  property("option 'andThen' does not replace the value when the original is empty") {
    import monadics.instances.option._

    forAll { (a: Int) =>
      Option.empty[Int].andThen(Option(a)) shouldBe Option.empty[Int]
    }
  }

  property("option 'andThen' replaces the value with an empty if the new value is empty") {
    import monadics.instances.option._

    forAll { (a: Int) =>
      Option(a).andThen(Option.empty[Int]) shouldBe Option.empty[Int]
    }
  }

  property("option 'traverse' should invert the Option and List") {
    import monadics.instances.option._
    import monadics.instances.list._

    forAll { (x: Int, f: Int => List[String]) =>
      Option(x).traverse(f) shouldBe f(x).map(Option(_))
    }
  }

  property("option 'traverse' should not invert when the value is empty") {
    import monadics.instances.option._
    import monadics.instances.list._

    forAll { (f: Int => List[String]) =>
      Option.empty[Int].traverse(f) shouldBe List(Option.empty[String])
    }
  }

  property("option 'sequence' should invert the list in an option value") {
    import monadics.instances.option._
    import monadics.instances.list._

    forAll { (list: List[Int]) =>
      Option(list).sequence[List, Int] shouldBe list.map(Option(_))
    }
  }

  property("option 'sequence' should not invert an empty option") {
    import monadics.instances.option._
    import monadics.instances.list._

    Option.empty[List[Int]].sequence[List, Int] shouldBe List(Option.empty[Int])
  }
}
