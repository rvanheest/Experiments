package monadics.test.laws

import monadics.ScalaMonads.{listIsMonadPlus, optionIsMonadPlusAndMonadFail, tryIsMonadPlusAndMonadFail}
import monadics.instances._
import monadics.laws.MonadPlusLaws
import monadics.structures.MonadPlus
import org.scalacheck.Arbitrary

import scala.language.higherKinds
import scala.util.Try

trait MonadPlusLawsSpec[MP[_]] extends MonadLawsSpec[MP] {

	override val laws = MonadPlusLaws[MP]
	implicit val instance: MonadPlus[MP]

	property(s"$name - monadplus left empty") {
		forAll { (mp: MP[Int]) =>
			laws.monadPlusLeftEmpty(mp)
		}
	}

	property(s"$name - monadPlus right empty") {
		forAll { (mp: MP[Int]) =>
			laws.monadPlusRightEmpty(mp)
		}
	}

	property(s"$name - monadPlus associativity") {
		forAll { (mpA: MP[Int], mpB: MP[Int], mpC: MP[Int]) =>
			laws.monadPlusAssociativity(mpA, mpB, mpC)
		}
	}

	property(s"$name - monadPlus empty flatMap") {
		forAll { (f: Int => MP[String]) =>
			laws.monadPlusEmptyFlatMap(f)
		}
	}

	property(s"$name - monadPlus andThen empty") {
		forAll { (altA: MP[Int], altB: MP[Int], altC: MP[Int]) =>
			laws.monadPlusAndThenEmpty(altA)
		}
	}
}

abstract class AbstractMonadPlusLawsSpec[MP[_]](override val name: String)
																			 (implicit override val instance: MonadPlus[MP],
																				override val arbIntInstance: Arbitrary[MP[Int]],
																				override val arbIntToStringInstance: Arbitrary[MP[Int => String]],
																				override val arbIntToMonadStringInstance: Arbitrary[Int => MP[String]],
																				override val arbStringToMonadLongInstance: Arbitrary[String => MP[Long]])
	extends MonadPlusLawsSpec[MP]

class ListMonadPlusSpec extends AbstractMonadPlusLawsSpec[List]("List")
class OptionMonadPlusSpec extends AbstractMonadPlusLawsSpec[Option]("Option")
class TryMonadPlusSpec extends AbstractMonadPlusLawsSpec[Try]("Try")
class OptionTMonadPlusSpec extends AbstractMonadPlusLawsSpec[OptionT[List, ?]]("OptionT[List, ?]")
class StateTMonadPlusSpec extends AbstractMonadPlusLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
