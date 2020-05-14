package monadics.test.laws

import monadics.instances.list._
import monadics.instances.monoids.values._
import monadics.instances.option._
import monadics.instances.{OptionT, StateT}
import monadics.laws.MonoidKLaws
import monadics.structures.{Equals, MonoidK}
import org.scalacheck.Arbitrary

import scala.language.higherKinds

trait MonoidKLawsSpec[MK[_]] extends SemigroupKLawsSpec[MK] {

  implicit val instance: MonoidK[MK]
  implicit val arbIntInstance: Arbitrary[MK[Int]]

  override val laws: MonoidKLaws[MK] = MonoidKLaws[MK]

  property(s"$name - monoidK right identity") {
    forAll { (a: MK[Int]) =>
      laws.combineRightIdentity(a).isEqual shouldBe true
    }
  }

  property(s"$name - monoidK left identity") {
    forAll { (a: MK[Int]) =>
      laws.combineLeftIdentity(a).isEqual shouldBe true
    }
  }
}

abstract class AbstractMonoidKLawsSpec[MK[_]](override val name: String)
                                             (implicit override val instance: MonoidK[MK],
                                              override val arbIntInstance: Arbitrary[MK[Int]],
                                              override val eqInt: Equals[MK[Int]])
  extends MonoidKLawsSpec[MK]

class ListOfIntMonoidKSpec extends AbstractMonoidKLawsSpec[List]("List")
class OptionOfMonoidMonoidKSpec extends AbstractMonoidKLawsSpec[Option]("Option")
class OptionTMonoidKSpec extends AbstractMonoidKLawsSpec[OptionT[List, ?]]("OptionT[List, ?]")
class StateTMonoidKSpec extends AbstractMonoidKLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
