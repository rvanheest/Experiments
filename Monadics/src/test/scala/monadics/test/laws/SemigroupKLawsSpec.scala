package monadics.test.laws

import monadics.instances.list._
import monadics.instances.monoids.values._
import monadics.instances.option._
import monadics.instances.{OptionT, StateT}
import monadics.laws.SemigroupKLaws
import monadics.structures.{Equals, SemigroupK}
import org.scalacheck.Arbitrary

import scala.language.higherKinds

trait SemigroupKLawsSpec[SK[_]] extends LawSpec {

  implicit val instance: SemigroupK[SK]
  implicit val arbIntInstance: Arbitrary[SK[Int]]
  implicit val eqInt: Equals[SK[Int]]

  val laws: SemigroupKLaws[SK] = SemigroupKLaws[SK]

  property(s"$name - semigroupK associativity") {
    forAll { (a: SK[Int], b: SK[Int], c: SK[Int]) =>
      laws.associativity(a, b, c).isEqual shouldBe true
    }
  }
}

abstract class AbstractSemigroupKLawsSpec[SK[_]](override val name: String)
                                                (implicit override val instance: SemigroupK[SK],
                                                 override val arbIntInstance: Arbitrary[SK[Int]],
                                                 override val eqInt: Equals[SK[Int]])
  extends SemigroupKLawsSpec[SK]

class ListOfIntSemigroupKSpec extends AbstractSemigroupKLawsSpec[List]("List")
class OptionOfSemigroupKSpec extends AbstractSemigroupKLawsSpec[Option]("Option")
class OptionTSemigroupKSpec extends AbstractSemigroupKLawsSpec[OptionT[List, ?]]("OptionT[List, ?]")
class StateTSemigroupKSpec extends AbstractSemigroupKLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
