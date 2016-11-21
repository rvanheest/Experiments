package monadics.test.laws

import monadics.ScalaMonoids.stringIsMonoid
import monadics.instances.either._
import monadics.instances.list._
import monadics.instances.option._
import monadics.instances.{NonEmptyList, Product, Sum}
import monadics.laws.SemigroupLaws
import monadics.structures.Semigroup
import org.scalacheck.Arbitrary

trait SemigroupLawsSpec[S] extends LawSpec {

  implicit val instance: Semigroup[S]
  implicit val arbInstance: Arbitrary[S]

  val laws = SemigroupLaws[S]

  property(s"$name - associativity") {
    forAll { (a: S, b: S, c: S) =>
      laws.associativity(a, b, c)
    }
  }
}

abstract class AbstractSemigroupLawsSpec[S](override val name: String)
                                           (implicit override val instance: Semigroup[S],
                                            override val arbInstance: Arbitrary[S])
  extends SemigroupLawsSpec[S]

class StringSemigroupSpec extends AbstractSemigroupLawsSpec[String]("String")
class SumOfIntSemigroupSpec extends AbstractSemigroupLawsSpec[Sum[Int]]("Sum of Int")
class ProductOfIntSemigroupSpec extends AbstractSemigroupLawsSpec[Product[Int]]("Product of Int")
class ListOfIntSemigroupSpec extends AbstractSemigroupLawsSpec[List[Int]]("List of Int")
class OptionOfSemigroupSpec extends AbstractSemigroupLawsSpec[Option[String]]("Option of Monoid")
class NonEmptyListSemigroupSpec extends AbstractSemigroupLawsSpec[NonEmptyList[Int]]("NonEmptyList of Int")
class EitherSemigroupSpec extends AbstractSemigroupLawsSpec[Either[String, Int]]("Either")
