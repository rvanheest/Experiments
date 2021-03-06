package monadics.test.laws

import monadics.instances._
import monadics.instances.either._
import monadics.instances.list._
import monadics.instances.monoids.values._
import monadics.instances.option._
import monadics.instances.tryMonad._
import monadics.laws.TraverseLaws
import monadics.structures.{Equals, Traverse}
import org.scalacheck.Arbitrary

import scala.language.higherKinds
import scala.util.Try

trait TraverseLawsSpec[T[_]] extends FunctorLawsSpec[T] {

  implicit val instance: Traverse[T]
  implicit val arbIntInstance: Arbitrary[T[Int]]
  implicit val arbListOfOptionOfInt: Arbitrary[T[List[Option[Int]]]]
  implicit val arbStringToOptionOfLong: Arbitrary[String => Option[Long]]
  implicit val arbIntToListOfString: Arbitrary[Int => List[String]]

  override val laws: TraverseLaws[T] = TraverseLaws[T]

  property(s"$name - traverse identity") {
    forAll { (xs: T[Int]) =>
      laws.traverseIdentity(xs).isEqual shouldBe true
    }
  }

  property(s"$name - traverse composition") {
    forAll(sizeRange(5)) { (xs: T[Int], g: String => Option[Long], f: Int => List[String]) =>
      laws.traverseComposition(xs, g, f).isEqual shouldBe true
    }
  }

  property(s"$name - sequence identity") {
    forAll { (xs: T[Int]) =>
      laws.sequenceIdentity(xs).isEqual shouldBe true
    }
  }

  property(s"$name - sequence composition") {
    forAll(sizeRange(10)) { (xs: T[List[Option[Int]]]) =>
      laws.sequenceComposition(xs).isEqual shouldBe true
    }
  }
}

abstract class AbstractTraverseLawsSpec[T[_]](override val name: String)
                                             (implicit override val instance: Traverse[T],
                                              override val arbIntInstance: Arbitrary[T[Int]],
                                              override val arbListOfOptionOfInt: Arbitrary[T[List[Option[Int]]]],
                                              override val arbStringToOptionOfLong: Arbitrary[String => Option[Long]],
                                              override val arbIntToListOfString: Arbitrary[Int => List[String]],
                                              override val eqInt: Equals[T[Int]],
                                              override val eqLong: Equals[T[Long]])
  extends TraverseLawsSpec[T]

class ListTraverseSpec extends AbstractTraverseLawsSpec[List]("List")
class OptionTraverseSpec extends AbstractTraverseLawsSpec[Option]("Option")
class TryTraverseSpec extends AbstractTraverseLawsSpec[Try]("Try")
class EitherTraverseSpec extends AbstractTraverseLawsSpec[Either[Int, ?]]("Either")
class EitherLeftTraverseSpec extends AbstractTraverseLawsSpec[LeftEither[?, Int]]("LeftEither")
class EitherRightTraverseSpec extends AbstractTraverseLawsSpec[RightEither[Int, ?]]("RightEither")
class NonEmptyListTraverseSpec extends AbstractTraverseLawsSpec[NonEmptyList]("NonEmptyList")
class WriterTraverseSpec extends AbstractTraverseLawsSpec[Writer[String, ?]]("Writer[String, ?]")
