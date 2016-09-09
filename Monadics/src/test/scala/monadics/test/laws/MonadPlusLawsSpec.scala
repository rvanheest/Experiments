package monadics.test.laws

import monadics.ScalaMonads.{listIsMonadPlus, optionIsMonadPlus, tryIsMonadPlus}
import monadics.instances._
import monadics.laws.MonadPlusLaws
import monadics.structures.MonadPlus
import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

import scala.language.higherKinds
import scala.util.Try

abstract class MonadPlusLawsSpec[MP[_]](name: String)
																					(implicit monadPlus: MonadPlus[MP],
																					 arbMPInt: Arbitrary[MP[Int]],
																					 arbIntToMPString: Arbitrary[Int => MP[String]])
	extends PropSpec with PropertyChecks with Matchers {

	val laws = MonadPlusLaws[MP]

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

class ListMonadPlusSpec extends MonadPlusLawsSpec[List]("List")

class OptionMonadPlusSpec extends MonadPlusLawsSpec[Option]("Option")

class TryMonadPlusSpec extends MonadPlusLawsSpec[Try]("Try")

class OptionTMonadPlusSpec extends MonadPlusLawsSpec[OptionT[List, ?]]("OptionT[List, ?]")

class StateTMonadPlusSpec extends MonadPlusLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
