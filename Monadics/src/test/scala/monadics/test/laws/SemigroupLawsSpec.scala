package monadics.test.laws

import monadics.instances.NonEmptyList
import monadics.instances.either._
import monadics.instances.list._
import monadics.instances.map._
import monadics.instances.monoids._
import monadics.instances.monoids.values._
import monadics.instances.option._
import monadics.laws.SemigroupLaws
import monadics.structures.{Equals, Semigroup}
import org.scalacheck.Arbitrary

trait SemigroupLawsSpec[S] extends LawSpec {

  implicit val instance: Semigroup[S]
  implicit val arbInstance: Arbitrary[S]
  implicit val eqS: Equals[S]

  val laws: SemigroupLaws[S] = SemigroupLaws[S]

  property(s"$name - associativity") {
    forAll { (a: S, b: S, c: S) =>
      laws.associativity(a, b, c).isEqual shouldBe true
    }
  }
}

abstract class AbstractSemigroupLawsSpec[S](override val name: String)
                                           (implicit override val instance: Semigroup[S],
                                            override val arbInstance: Arbitrary[S],
                                            override val eqS: Equals[S])
  extends SemigroupLawsSpec[S]

class ByteSemigroupSpec extends AbstractSemigroupLawsSpec[Byte]("Byte")
class ShortSemigroupSpec extends AbstractSemigroupLawsSpec[Short]("Short")
class IntSemigroupSpec extends AbstractSemigroupLawsSpec[Int]("Int")
class LongSemigroupSpec extends AbstractSemigroupLawsSpec[Long]("Long")
class StringSemigroupSpec extends AbstractSemigroupLawsSpec[String]("String")
class SumOfIntSemigroupSpec extends AbstractSemigroupLawsSpec[Sum[Int]]("Sum of Int")
class ProductOfIntSemigroupSpec extends AbstractSemigroupLawsSpec[Product[Int]]("Product of Int")
class AnySemigroupSpec extends AbstractSemigroupLawsSpec[Any]("Any")
class AllSemigroupSpec extends AbstractSemigroupLawsSpec[All]("All")
class DualSemigroupSpec extends AbstractSemigroupLawsSpec[Dual[String]]("Dual of Monoid")
class EndoSemigroupSpec extends AbstractSemigroupLawsSpec[Endo[Int]]("Endo of Int")
class ListOfIntSemigroupSpec extends AbstractSemigroupLawsSpec[List[Int]]("List of Int")
class OptionOfSemigroupSpec extends AbstractSemigroupLawsSpec[Option[String]]("Option of Monoid")
class NonEmptyListSemigroupSpec extends AbstractSemigroupLawsSpec[NonEmptyList[Int]]("NonEmptyList of Int")
class EitherSemigroupSpec extends AbstractSemigroupLawsSpec[Either[String, Int]]("Either")
class MapOfStringToListOfIntSemigroupSpec extends AbstractSemigroupLawsSpec[Map[String, List[Int]]]("Map of String to List of Int")
