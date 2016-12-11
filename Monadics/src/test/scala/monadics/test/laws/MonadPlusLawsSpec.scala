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

	property(s"$name - monadplus left distributivity") {
		forAll { (mpX: MP[Int], mpY: MP[Int], f: Int => MP[String]) =>
			laws.monadCombineLeftDistributivity(mpX, mpY, f).isEqual shouldBe true
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
class OptionMonadPlusSpec extends AbstractMonadPlusLawsSpec[Option]("Option")
class OptionTMonadPlusSpec extends AbstractMonadPlusLawsSpec[OptionT[List, ?]]("OptionT[List, ?]")
class StateTMonadPlusSpec extends AbstractMonadPlusLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
