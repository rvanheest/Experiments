package monadics.test.instances

class EitherSpec extends InstanceSpec {

  property("either 'orElse' returns the second when the first is Left") {
    import monadics.instances.either._

    forAll { (x: Int, eY: Either[Int, String]) =>
      Left[Int, String](x) orElse eY shouldBe eY
    }
  }

  property("either 'orElse' returns the first when it is Right") {
    import monadics.instances.either._

    forAll { (x: String, eY: Either[Int, String]) =>
      Right[Int, String](x) orElse eY shouldBe Right(x)
    }
  }

  property("either 'as' replaces a Right with the other value") {
    import monadics.instances.either._

    forAll { (x: String, b: Boolean) =>
      Right[Int, String](x) as b shouldBe Right(b)
    }
  }

  property("either 'as' does not replaces a Left with the other value") {
    import monadics.instances.either._

    forAll { (x: Int, b: Boolean) =>
      Left(x) as b shouldBe Left(x)
    }
  }

  property("either 'void' replaces a Right with ()") {
    import monadics.instances.either._

    forAll { (x: String) =>
      Right[Int, String](x).void shouldBe Right(())
    }
  }

  property("either 'void' does not replaces a Left with ()") {
    import monadics.instances.either._

    forAll { (x: Int) =>
      Left(x).void shouldBe Left(x)
    }
  }

  property("either 'zipWith' merges the Right value with the result of applying the value to the given function") {
    import monadics.instances.either._

    forAll { (x: String, f: String => Int) =>
      Right[Int, String](x).zipWith(f) shouldBe Right(x, f(x))
    }
  }

  property("either 'zipWith' does not merge the Left value with the result of applying the value to the given function") {
    import monadics.instances.either._

    forAll { (x: Int, f: String => Int) =>
      Left(x).zipWith(f) shouldBe Left(x)
    }
  }

  property("either 'traverse' switches Option and Right") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: String) =>
      Right[Int, String](x).traverse[Option, String](Some(_)) shouldBe Some(Right(x))
    }
  }

  property("either 'traverse' converts an empty option inside a Right into an empty option") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: String) =>
      Right[Int, String](x).traverse[Option, String](_ => None) shouldBe None
    }
  }

  property("either 'traverse' does not switch Option and Left") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: Int) =>
      Left[Int, String](x).traverse[Option, String](Some(_)) shouldBe Some(Left(x))
    }
  }

  property("either 'traverse' does not convert an empty option with a Left into an empty option") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: Int) =>
      Left[Int, String](x).traverse[Option, String](_ => None) shouldBe Some(Left(x))
    }
  }

  property("either 'sequence' switches Option and Right") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: String) =>
      Right[Int, Option[String]](Some(x)).sequence shouldBe Some(Right(x))
    }
  }

  property("either 'sequence' coverts an empty option inside a Right into an empty option") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: String) =>
      Right[Int, Option[String]](None).sequence shouldBe None
    }
  }

  property("either 'sequence' does not switch Option and Left") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: Int) =>
      Left[Int, Option[String]](x).sequence shouldBe Some(Left(x))
    }
  }
}

class EitherLeftSpec extends InstanceSpec {

  property("either 'orElse' returns the second when the first is Right") {
    import monadics.instances.either._

    forAll { (x: String, eY: Either[Int, String]) =>
      Right[Int, String](x).left orElse eY.left shouldBe eY.left
    }
  }

  property("either 'orElse' returns the first when it is Left") {
    import monadics.instances.either._

    forAll { (x: Int, eY: Either[Int, String]) =>
      Left[Int, String](x).left orElse eY.left shouldBe Left(x).left
    }
  }

  property("either 'as' replaces a Left with the other value") {
    import monadics.instances.either._

    forAll { (x: Int, b: Boolean) =>
      Left[Int, String](x).left as b shouldBe Left(b).left
    }
  }

  property("either 'as' does not replaces a Right with the other value") {
    import monadics.instances.either._

    forAll { (x: String, b: Boolean) =>
      Right[Int, String](x).left as b shouldBe Right(x).left
    }
  }

  property("either 'void' replaces a Left with ()") {
    import monadics.instances.either._

    forAll { (x: Int) =>
      Left[Int, String](x).left.void shouldBe Left(()).left
    }
  }

  property("either 'void' does not replaces a Right with ()") {
    import monadics.instances.either._

    forAll { (x: String) =>
      Right[Int, String](x).left.void shouldBe Right(x).left
    }
  }

  property("either 'zipWith' merges the Left value with the result of applying the value to the given function") {
    import monadics.instances.either._

    forAll { (x: Int, f: Int => String) =>
      Left[Int, String](x).left.zipWith(f) shouldBe Left(x, f(x)).left
    }
  }

  property("either 'zipWith' does not merge the Right value with the result of applying the value to the given function") {
    import monadics.instances.either._

    forAll { (x: String, f: Int => String) =>
      Right[Int, String](x).left.zipWith(f) shouldBe Right(x).left
    }
  }

  property("either 'traverse' switches Option and Left") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: Int) =>
      Left[Int, String](x).left.traverse[Option, Int](Some(_)) shouldBe Some(Left(x).left)
    }
  }

  property("either 'traverse' converts an empty option inside a Left into an empty option") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: Int) =>
      Left[Int, String](x).left.traverse[Option, Int](_ => None) shouldBe None
    }
  }

  property("either 'traverse' does not switch Option and Right") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: String) =>
      Right[Int, String](x).left.traverse[Option, Int](Some(_)) shouldBe Some(Right(x).left)
    }
  }

  property("either 'traverse' does not convert an empty option with a Right into an empty option") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: String) =>
      Right[Int, String](x).left.traverse[Option, Int](_ => None) shouldBe Some(Right(x).left)
    }
  }

  property("either 'sequence' switches Option and Left") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: Int) =>
      Left[Option[Int], String](Some(x)).left.sequence shouldBe Some(Left(x).left)
    }
  }

  property("either 'sequence' coverts an empty option inside a Left into an empty option") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: String) =>
      Left[Option[Int], String](None).left.sequence shouldBe None
    }
  }

  property("either 'sequence' does not switch Option and Right") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: String) =>
      Right[Option[Int], String](x).left.sequence shouldBe Some(Right(x).left)
    }
  }
}

class EitherRightSpec extends InstanceSpec {

  property("either 'orElse' returns the second when the first is Left") {
    import monadics.instances.either._

    forAll { (x: Int, eY: Either[Int, String]) =>
      Left[Int, String](x).right orElse eY.right shouldBe eY.right
    }
  }

  property("either 'orElse' returns the first when it is Right") {
    import monadics.instances.either._

    forAll { (x: String, eY: Either[Int, String]) =>
      Right[Int, String](x).right orElse eY.right shouldBe Right(x).right
    }
  }

  property("either 'as' replaces a Right with the other value") {
    import monadics.instances.either._

    forAll { (x: String, b: Boolean) =>
      Right[Int, String](x).right as b shouldBe Right(b).right
    }
  }

  property("either 'as' does not replaces a Left with the other value") {
    import monadics.instances.either._

    forAll { (x: Int, b: Boolean) =>
      Left(x).right as b shouldBe Left(x).right
    }
  }

  property("either 'void' replaces a Right with ()") {
    import monadics.instances.either._

    forAll { (x: String) =>
      Right[Int, String](x).right.void shouldBe Right(()).right
    }
  }

  property("either 'void' does not replaces a Left with ()") {
    import monadics.instances.either._

    forAll { (x: Int) =>
      Left(x).right.void shouldBe Left(x).right
    }
  }

  property("either 'zipWith' merges the Right value with the result of applying the value to the given function") {
    import monadics.instances.either._

    forAll { (x: String, f: String => Int) =>
      Right[Int, String](x).right.zipWith(f) shouldBe Right(x, f(x)).right
    }
  }

  property("either 'zipWith' does not merge the Left value with the result of applying the value to the given function") {
    import monadics.instances.either._

    forAll { (x: Int, f: String => Int) =>
      Left(x).right.zipWith(f) shouldBe Left(x).right
    }
  }

  property("either 'traverse' switches Option and Right") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: String) =>
      Right[Int, String](x).right.traverse[Option, String](Some(_)) shouldBe Some(Right(x).right)
    }
  }

  property("either 'traverse' converts an empty option inside a Right into an empty option") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: String) =>
      Right[Int, String](x).right.traverse[Option, String](_ => None) shouldBe None
    }
  }

  property("either 'traverse' does not switch Option and Left") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: Int) =>
      Left[Int, String](x).right.traverse[Option, String](Some(_)) shouldBe Some(Left(x).right)
    }
  }

  property("either 'traverse' does not convert an empty option with a Left into an empty option") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: Int) =>
      Left[Int, String](x).right.traverse[Option, String](_ => None) shouldBe Some(Left(x).right)
    }
  }

  property("either 'sequence' switches Option and Right") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: String) =>
      Right[Int, Option[String]](Some(x)).right.sequence shouldBe Some(Right(x).right)
    }
  }

  property("either 'sequence' coverts an empty option inside a Right into an empty option") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: String) =>
      Right[Int, Option[String]](None).right.sequence shouldBe None
    }
  }

  property("either 'sequence' does not switch Option and Left") {
    import monadics.instances.either._
    import monadics.instances.option._

    forAll { (x: Int) =>
      Left[Int, Option[String]](x).right.sequence shouldBe Some(Left(x).right)
    }
  }
}
