package monadics.test.laws

import monadics.instances._
import monadics.instances.list._
import monadics.instances.option._
import monadics.instances.monoids.values._
import monadics.laws.MonadPlusLaws
import monadics.structures.{Equals, MonadPlus}
import org.scalacheck.Arbitrary

import scala.language.higherKinds

trait MonadPlusLawsSpec[MP[_]] extends MonadFilterLawsSpec[MP] with AlternativeLawsSpec[MP] {

	override val laws: MonadPlusLaws[MP] = MonadPlusLaws[MP]
	implicit val instance: MonadPlus[MP]
}

trait MonadPlusLeftDistributivityLaw[MP[_]] extends MonadPlusLawsSpec[MP] {
	property(s"$name - monadplus left distributivity") {
		forAll { (mpX: MP[Int], mpY: MP[Int], f: Int => MP[String]) =>
			laws.monadplusCombineLeftDistributivity(mpX, mpY, f).isEqual shouldBe true
		}
	}
}

trait MonadPlusLeftCatchLaw[MP[_]] extends MonadPlusLawsSpec[MP] {
	property(s"$name - monadplus left catch") {
		forAll { (a: Int, mpA: MP[Int]) =>
			laws.monadplusLeftCatch(a, mpA).isEqual shouldBe true
		}
	}
}

abstract class AbstractMonadPlusLawsSpec[MP[_]](override val name: String)
																							 (implicit override val instance: MonadPlus[MP],
																								override val arbIntInstance: Arbitrary[MP[Int]],
																								override val arbIntToStringInstance: Arbitrary[MP[Int => String]],
																								override val arbIntToMonadStringInstance: Arbitrary[Int => MP[String]],
																								override val arbStringToMonadLongInstance: Arbitrary[String => MP[Long]],
																								override val arbStringToLongInstance: Arbitrary[MP[String => Long]],
																								override val eqInt: Equals[MP[Int]],
																								override val eqLong: Equals[MP[Long]],
																								override val eqString: Equals[MP[String]])
	extends MonadPlusLawsSpec[MP]

class ListMonadPlusSpec extends AbstractMonadPlusLawsSpec[List]("List")
																with MonadPlusLeftDistributivityLaw[List]
class OptionMonadPlusSpec extends AbstractMonadPlusLawsSpec[Option]("Option")
																	with MonadPlusLeftCatchLaw[Option] // left distributivity does not hold for Option
class OptionTMonadPlusSpec extends AbstractMonadPlusLawsSpec[OptionT[List, ?]]("OptionT[List, ?]")
																	 with MonadPlusLeftDistributivityLaw[OptionT[List, ?]]
class StateTMonadPlusSpec extends AbstractMonadPlusLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
																	with MonadPlusLeftDistributivityLaw[StateT[Int, ?, List]]
