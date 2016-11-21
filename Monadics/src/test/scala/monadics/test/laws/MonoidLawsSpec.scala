package monadics.test.laws

import monadics.ScalaMonoids.stringIsMonoid
import monadics.instances.list._
import monadics.instances.option._
import monadics.instances.{Product, Sum}
import monadics.laws.MonoidLaws
import monadics.structures.Monoid
import org.scalacheck.Arbitrary

trait MonoidLawsSpec[M] extends SemigroupLawsSpec[M] {

  implicit val instance: Monoid[M]
  implicit val arbInstance: Arbitrary[M]

  override val laws = MonoidLaws[M]

  property(s"$name - right identity") {
    forAll { (a: M) =>
      laws.combineRightIdentity(a)
    }
  }

  property(s"$name - left identity") {
    forAll { (a: M) =>
      laws.combineLeftIdentity(a)
    }
  }
}

abstract class AbstractMonoidLawsSpec[M](override val name: String)
                                        (implicit override val instance: Monoid[M],
                                         override val arbInstance: Arbitrary[M])
  extends MonoidLawsSpec[M]

class StringMonoidSpec extends AbstractMonoidLawsSpec[String]("String")
class SumOfIntMonoidSpec extends AbstractMonoidLawsSpec[Sum[Int]]("Sum of Int")
class ProductOfIntMonoidSpec extends AbstractMonoidLawsSpec[Product[Int]]("Product of Int")
class ListOfIntMonoidSpec extends AbstractMonoidLawsSpec[List[Int]]("List of Int")
class OptionOfSemigroupMonoidSpec extends AbstractMonoidLawsSpec[Option[String]]("Option of Monoid")
