package monadics.test.laws

import monadics.instances._
import monadics.instances.list._
import monadics.instances.monoids.values._
import monadics.instances.option._
import monadics.laws.AlternativeLaws
import monadics.structures.{Alternative, Equals}
import org.scalacheck.Arbitrary

import scala.language.higherKinds

trait AlternativeLawsSpec[Alt[_]] extends ApplicativeLawsSpec[Alt] with MonoidKLawsSpec[Alt] {

	override val laws: AlternativeLaws[Alt] = AlternativeLaws[Alt]
	implicit val instance: Alternative[Alt]

	property(s"$name - alternative right distributivity") {
		forAll { (altA: Alt[Int], altF: Alt[Int => String], altG: Alt[Int => String]) =>
			laws.alternativeRightDistributivity(altA, altF, altG).isEqual shouldBe true
		}
	}

	property(s"$name - alternative right absorption") {
		forAll { (altF: Alt[Int => String]) =>
			laws.alternativeRightAbsorption(altF).isEqual shouldBe true
		}
	}

	property(s"$name - alternative left distributivity") {
		forAll { (altX: Alt[Int], altY: Alt[Int], f: Int => String) =>
			laws.alternativeLeftDistributivity(altX, altY, f).isEqual shouldBe true
		}
	}
}

abstract class AbstractAlternativeLawsSpec[Alt[_]](override val name: String)
																									(implicit override val instance: Alternative[Alt],
																									 override val arbIntInstance: Arbitrary[Alt[Int]],
																									 override val arbIntToStringInstance: Arbitrary[Alt[Int => String]],
																									 override val arbStringToLongInstance: Arbitrary[Alt[String => Long]],
																									 override val eqInt: Equals[Alt[Int]],
																									 override val eqLong: Equals[Alt[Long]],
																									 override val eqString: Equals[Alt[String]])
	extends AlternativeLawsSpec[Alt]

class ListAlternativeSpec extends AbstractAlternativeLawsSpec[List]("List")
class OptionAlternativeSpec extends AbstractAlternativeLawsSpec[Option]("Option")
class OptionTAlternativeSpec extends AbstractAlternativeLawsSpec[OptionT[List, ?]]("OptionT[List, ?]")
class StateTAlternativeSpec extends AbstractAlternativeLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
