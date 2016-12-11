package monadics.test.laws

import monadics.instances._
import monadics.instances.list._
import monadics.instances.monoids.values._
import monadics.instances.option._
import monadics.laws.MonadFilterLaws
import monadics.structures.{Equals, MonadFilter}
import org.scalacheck.Arbitrary

import scala.language.higherKinds

trait MonadFilterLawsSpec[MF[_]] extends MonadLawsSpec[MF] {

  override val laws: MonadFilterLaws[MF] = MonadFilterLaws[MF]
  implicit val instance: MonadFilter[MF]

  property(s"$name - monadfilter left distributivity") {
    forAll { (f: Int => MF[String]) =>
      laws.monadFilterLeftDistributivity(f).isEqual shouldBe true
    }
  }

  property(s"$name - monadfilter right distributivity") {
    forAll { (mfa: MF[Int]) =>
      laws.monadFilterRightDistributivity(mfa).isEqual shouldBe true
    }
  }
}

abstract class AbstractMonadFilterLawsSpec[MF[_]](override val name: String)
                                                 (implicit override val instance: MonadFilter[MF],
                                                  override val arbIntInstance: Arbitrary[MF[Int]],
                                                  override val arbIntToStringInstance: Arbitrary[MF[Int => String]],
                                                  override val arbIntToMonadStringInstance: Arbitrary[Int => MF[String]],
                                                  override val arbStringToMonadLongInstance: Arbitrary[String => MF[Long]],
                                                  override val arbStringToLongInstance: Arbitrary[MF[String => Long]],
                                                  override val eqInt: Equals[MF[Int]],
                                                  override val eqLong: Equals[MF[Long]],
                                                  override val eqString: Equals[MF[String]])
  extends MonadFilterLawsSpec[MF]

class ListMonadFilterSpec extends AbstractMonadFilterLawsSpec[List]("List")
class OptionMonadFilterSpec extends AbstractMonadFilterLawsSpec[Option]("Option")
class OptionTMonadFilterSpec extends AbstractMonadFilterLawsSpec[OptionT[List, ?]]("OptionT[List, ?]")
class StateTMonadFilterSpec extends AbstractMonadFilterLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
