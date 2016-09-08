package monadics.test.laws

import monadics.ScalaMonads.{functionIsMonadPlus, listIsMonadPlus, optionIsMonadPlus, tryIsMonadPlus}
import monadics.instances._
import monadics.laws.ApplicativeLaws
import monadics.structures.Applicative
import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

import scala.language.higherKinds
import scala.util.Try

abstract class ApplicativeLawsSpec[App[_]](name: String)
																					(implicit applicative: Applicative[App],
																					 arbAppInt: Arbitrary[App[Int]],
																					 arbAppIntToString: Arbitrary[App[Int => String]])
	extends PropSpec with PropertyChecks with Matchers {

	val laws = ApplicativeLaws[App]

	property(s"$name - applicative identity") {
		forAll { (xs: App[Int]) =>
			laws.applicativeIdentity(xs)
		}
	}

	property(s"$name - applicative homomorphism") {
		forAll { (a: Int, f: Int => String) =>
			laws.applicativeHomomorphism(a, f)
		}
	}

	property(s"$name - applicative interchange") {
		forAll { (a: Int, appF: App[Int => String]) =>
			laws.applicativeInterchange(a, appF)
		}
	}

	property(s"$name - applicative map") {
		forAll { (appA: App[Int], f: Int => String) =>
			laws.applicativeMap(appA, f)
		}
	}
}

class ListApplicativeSpec extends ApplicativeLawsSpec[List]("List")
class OptionApplicativeSpec extends ApplicativeLawsSpec[Option]("Option")
class TryApplicativeSpec extends ApplicativeLawsSpec[Try]("Try")
class FunctionApplicativeSpec extends ApplicativeLawsSpec[Int => ?]("Int => ?")
class IdentityApplicativeSpec extends ApplicativeLawsSpec[Identity]("Identity")
class OptionTApplicativeSpec extends ApplicativeLawsSpec[OptionT[List, ?]]("OptionT[List, ?]")
class StateApplicativeSpec extends ApplicativeLawsSpec[State[Int, ?]]("State[Int, ?]")
class StateTApplicativeSpec extends ApplicativeLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
class TreeApplicativeSpec extends ApplicativeLawsSpec[Tree]("Tree")
