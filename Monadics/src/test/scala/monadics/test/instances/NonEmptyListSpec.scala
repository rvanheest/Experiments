package monadics.test.instances

import scala.language.postfixOps

class NonEmptyListSpec extends InstanceSpec {

  property("nonemptylist 'map'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int], f: Int => String) =>
      NonEmptyList(x, xs).map(f) shouldBe NonEmptyList(f(x), xs map f)
    }
  }

  property("nonemptylist 'as'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int], b: String) =>
      NonEmptyList(x, xs).as(b) shouldBe NonEmptyList(b, List.fill(xs.size)(b))
    }
  }

  property("nonemptylist 'void'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int]) =>
      NonEmptyList(x, xs).void shouldBe NonEmptyList((), List.fill(xs.size)(()))
    }
  }

  property("nonemptylist 'zipWith'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int], f: Int => String) =>
      NonEmptyList(x, xs).zipWith(f) shouldBe NonEmptyList((x, f(x)), xs.map(x => (x, f(x))))
    }
  }

  property("nonemptylist '<*>'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int], fx: Int => String, fxs: List[Int => String]) =>
      val y :: ys = (fx :: fxs).flatMap(f => (x :: xs).map(f))
      NonEmptyList(fx, fxs) <*> NonEmptyList(x, xs) shouldBe NonEmptyList(y, ys)
    }
  }

  property("nonemptylist 'flatMap'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int], f: Int => NonEmptyList[String]) =>
      NonEmptyList(x, xs).flatMap(f) shouldBe xs.map(f).foldLeft(f(x))(_ ++ _)
    }
  }

  property("nonemptylist '++'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int], y: Int, ys: List[Int]) =>
      NonEmptyList(x, xs) ++ NonEmptyList(y, ys) shouldBe NonEmptyList(x, xs ::: List(y) ::: ys)
    }
  }

  property("nonemptylist 'foldLeft'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int]) =>
      NonEmptyList(x, xs).foldLeft(0)(_ + _) shouldBe xs.foldLeft(x)(_ + _)
    }
  }

  property("nonemptylist 'foldRight'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int]) =>
      NonEmptyList(x, xs).foldRight(List.empty[Int])(_ :: _) shouldBe x :: xs
    }
  }

  property("nonemptylist 'foldMap'") {
    import monadics.instances.NonEmptyList
    import monadics.instances.monoids.Sum

    forAll { (x: Int, xs: List[Int]) =>
      NonEmptyList(x, xs).foldMap(i => Sum(2 * i)).sum shouldBe (x :: xs).map(i => 2 * i).sum
    }
  }

  property("nonemptylist 'toList'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int]) =>
      NonEmptyList(x, xs).toList shouldBe x :: xs
    }
  }

  property("nonemptylist 'size'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int]) =>
      NonEmptyList(x, xs).size shouldBe 1 + xs.size

    }
  }

  property("nonemptylist 'contains'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int]) =>
      val nel = NonEmptyList(x, xs)
      nel.contains(x) && xs.forall(nel.contains) shouldBe true
    }
  }

  property("nonemptylist 'max'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int]) =>
      NonEmptyList(x, xs).max shouldBe (if (xs.isEmpty) x else math.max(x, xs.max))
    }
  }

  property("nonemptylist 'min'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int]) =>
      NonEmptyList(x, xs).min shouldBe (if (xs.isEmpty) x else math.min(x, xs.min))
    }
  }

  property("nonemptylist 'sum'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int]) =>
      NonEmptyList(x, xs).sum shouldBe x + xs.sum
    }
  }

  property("nonemptylist 'product'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int]) =>
      NonEmptyList(x, xs).product shouldBe x * xs.product
    }
  }

  property("nonemptylist 'all'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Boolean, xs: List[Boolean]) =>
      NonEmptyList(x, xs).all shouldBe x && xs.forall(identity)
    }
  }

  property("nonemptylist 'any'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Boolean, xs: List[Boolean]) =>
      NonEmptyList(x, xs).any shouldBe x || xs.exists(identity)
    }
  }

  property("nonemptylist 'exists'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int]) =>
      val nel = NonEmptyList(x, xs)
      nel.exists(x ==) && xs.forall(a => nel.exists(a ==)) shouldBe true
    }
  }

  property("nonemptylist 'forall'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int]) =>
      NonEmptyList(x, xs).map(2 *).forall(_ % 2 == 0) shouldBe true
    }
  }

  property("nonemptylist 'find'") {
    import monadics.instances.NonEmptyList

    forAll { (x: Int, xs: List[Int]) =>
      val nel = NonEmptyList(x, xs)
      nel.find(x ==).exists(x ==) && xs.forall(a => nel.find(a ==).exists(a ==)) shouldBe true
    }
  }

  property("nonemptylist 'traverse'") {
    import monadics.instances.NonEmptyList
    import monadics.instances.option._

    forAll { (x: Int, xs: List[Int], f: Int => String) =>
      NonEmptyList(x, xs).traverse[Option, String](a => Option(f(a))) shouldBe Option(NonEmptyList(f(x), xs.map(f)))
    }
  }

  property("nonemptylist 'traverse' with a single None should return a None") {
    import monadics.instances.NonEmptyList
    import monadics.instances.option._

    forAll { (x: Int, xs: List[Int]) =>
      NonEmptyList(x, xs).traverse[Option, String](_ => Option.empty[String]) shouldBe Option.empty[String]
    }
  }

  property("nonemptylist 'sequence'") {
    import monadics.instances.NonEmptyList
    import monadics.instances.option._

    forAll { (x: Int, xs: List[Int]) =>
      val nel = NonEmptyList(Option(x), xs.map(Option(_)) ::: List(Option(x)) ::: xs.map(Option(_)))
      nel.sequence shouldBe Option(NonEmptyList(x, xs ::: List(x) ::: xs))
    }
  }

  property("nonemptylist 'sequence' with empty Option") {
    import monadics.instances.NonEmptyList
    import monadics.instances.option._

    forAll { (x: Int, xs: List[Int]) =>
      val nel = NonEmptyList(Option(x), xs.map(Option(_)) ::: List(Option.empty[Int]) ::: xs.map(Option(_)))
      nel.sequence shouldBe Option.empty[Int]
    }
  }
}
