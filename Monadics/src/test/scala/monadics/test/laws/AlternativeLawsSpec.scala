package monadics.test.laws

import monadics.instances._
import monadics.instances.list._
import monadics.instances.option._
import monadics.instances.tryMonad._
import monadics.instances.monoids.values._
import monadics.laws.AlternativeLaws
import monadics.structures.{Alternative, Equals}
import org.scalacheck.Arbitrary

import scala.language.higherKinds
import scala.util.Try

trait AlternativeLawsSpec[Alt[_]] extends ApplicativeLawsSpec[Alt] {

	override val laws: AlternativeLaws[Alt] = AlternativeLaws[Alt]
	implicit val instance: Alternative[Alt]

	property(s"$name - alternative left empty") {
		forAll { (alt: Alt[Int]) =>
			laws.alternativeLeftEmpty(alt).isEqual shouldBe true
		}
	}

	property(s"$name - alternative right empty") {
		forAll { (alt: Alt[Int]) =>
			laws.alternativeRightEmpty(alt).isEqual shouldBe true
		}
	}

	property(s"$name - alternative associativity") {
		forAll { (altA: Alt[Int], altB: Alt[Int], altC: Alt[Int]) =>
			laws.alternativeAssociativity(altA, altB, altC).isEqual shouldBe true
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
