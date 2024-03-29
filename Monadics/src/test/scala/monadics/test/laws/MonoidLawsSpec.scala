package monadics.test.laws

import monadics.instances.list._
import monadics.instances.map._
import monadics.instances.monoids.values._
import monadics.instances.monoids._
import monadics.instances.option._
import monadics.laws.MonoidLaws
import monadics.structures.{Equals, Monoid}
import org.scalacheck.Arbitrary

trait MonoidLawsSpec[M] extends SemigroupLawsSpec[M] {

  implicit val instance: Monoid[M]
  implicit val arbInstance: Arbitrary[M]

  override val laws: MonoidLaws[M] = MonoidLaws[M]

  property(s"$name - monoid right identity") {
    forAll { (a: M) =>
      laws.combineRightIdentity(a).isEqual shouldBe true
    }
  }

  property(s"$name - monoid left identity") {
    forAll { (a: M) =>
      laws.combineLeftIdentity(a).isEqual shouldBe true
    }
  }
}

abstract class AbstractMonoidLawsSpec[M](override val name: String)
                                        (implicit override val instance: Monoid[M],
                                         override val arbInstance: Arbitrary[M],
                                         override val eqS: Equals[M])
  extends MonoidLawsSpec[M]

class ByteMonoidSpec extends AbstractMonoidLawsSpec[Byte]("Byte")
class ShortMonoidSpec extends AbstractMonoidLawsSpec[Short]("Short")
class IntMonoidSpec extends AbstractMonoidLawsSpec[Int]("Int")
class LongMonoidSpec extends AbstractMonoidLawsSpec[Long]("Long")
class StringMonoidSpec extends AbstractMonoidLawsSpec[String]("String")
class SumOfIntMonoidSpec extends AbstractMonoidLawsSpec[Sum[Int]]("Sum of Int")
class ProductOfIntMonoidSpec extends AbstractMonoidLawsSpec[Product[Int]]("Product of Int")
class AnyMonoidSpec extends AbstractMonoidLawsSpec[Any]("Any")
class AllMonoidSpec extends AbstractMonoidLawsSpec[All]("All")
class DualMonoidSpec extends AbstractMonoidLawsSpec[Dual[String]]("Dual of Monoid")
class EndoMonoidSpec extends AbstractMonoidLawsSpec[Endo[Int]]("Endo of Int")
class ListOfIntMonoidSpec extends AbstractMonoidLawsSpec[List[Int]]("List of Int")
class OptionOfMonoidMonoidSpec extends AbstractMonoidLawsSpec[Option[String]]("Option of Monoid")
class MapOfStringToListOfIntMonoidSpec extends AbstractMonoidLawsSpec[Map[String, List[Int]]]("Map of String to List of Int")
