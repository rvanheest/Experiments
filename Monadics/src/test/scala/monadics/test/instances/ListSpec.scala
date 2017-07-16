package monadics.test.instances

class ListSpec extends InstanceSpec {

  property("list 'as' replaces the values with other values") {
    import monadics.instances.list._

    forAll { (xs: List[Int], v: String) =>
      xs.as(v) shouldBe List.fill(xs.size)(v)
    }
  }

  property("list 'void' replaces the values with ()") {
    import monadics.instances.list._

    forAll { (xs: List[Int]) =>
      xs.void shouldBe List.fill(xs.size)(())
    }
  }

  property("list 'zipWith' combines the values in the list with the function in tuples") {
    import monadics.instances.list._

    forAll { (xs: List[Int], f: Int => String) =>
      xs.zipWith(f) shouldBe xs.zip(xs.map(f))
    }
  }

  property("list 'applicative' should apply the functions in the left value to the values on the right") {
    import monadics.instances.list._

    forAll { (xs: List[Int], fs: List[Int => String]) =>
      fs <*> xs shouldBe fs.flatMap(xs.map(_))
    }
  }

  property("list 'foldMap' should map the elements and fold them together into a total sum") {
    import monadics.instances.list._
    import monadics.instances.monoids.Sum

    forAll { (xs: List[Int]) =>
      xs.foldMap(Sum(_)).sum shouldBe xs.sum
    }
  }

  property("list 'foldMap' should map the elements and fold them together into a total product") {
    import monadics.instances.list._
    import monadics.instances.monoids.Product

    forAll { (xs: List[Int]) =>
      xs.foldMap(Product(_)).product shouldBe xs.product
    }
  }

  property("list 'all' should return true if all elements are true") {
    import monadics.instances.list._

    List.fill(10)(true).all shouldBe true
  }

  property("list 'all' should return false if any element is false") {
    import monadics.instances.list._

    (List.fill(10)(true) ::: List(false) ::: List.fill(10)(true)).all shouldBe false
  }

  property("list 'any' should return true if any element is true") {
    import monadics.instances.list._

    (List.fill(10)(false) ::: List(true) ::: List.fill(10)(false)).any shouldBe true
  }

  property("list 'any' should return false if no elements is true") {
    import monadics.instances.list._

    List.fill(10)(false).any shouldBe false
  }

  def isEven(x: Int): Boolean = x % 2 == 0

  property("list 'traverse' with Option should return a Some if all values in traverse were successful") {
    import monadics.instances.list._
    import monadics.instances.option._

    forAll { (xs: List[Int]) =>
      xs.traverse[Option, Int](x => if (isEven(x * 2)) Some(x) else None) shouldBe Some(xs)
    }
  }

  property("list 'traverse' with Option should return a None if not all values in traverse were successful") {
    import monadics.instances.list._
    import monadics.instances.option._

    forAll { (xs: List[Int]) =>
      (1 :: xs).traverse[Option, Int](x => if (isEven(x)) Some(x) else None) shouldBe None
    }
  }

  property("list 'sequence' with Option switches the List and Option") {
    import monadics.instances.list._
    import monadics.instances.option._

    forAll { (xs: List[Int]) =>
      xs.map(Option(_)).sequence[Option, Int] shouldBe Some(xs)
    }
  }

  property("list 'sequence' with Option returns none if not all values were Some") {
    import monadics.instances.list._
    import monadics.instances.option._

    forAll { (xs: List[Int]) =>
      (xs.map(Option(_)) ::: List(Option.empty[Int]) ::: xs.map(Option(_))).sequence[Option, Int] shouldBe None
    }
  }
}
