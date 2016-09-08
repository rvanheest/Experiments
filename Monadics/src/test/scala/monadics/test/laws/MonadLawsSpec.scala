package monadics.test.laws

import monadics.ScalaMonads.{functionIsMonadPlus, listIsMonadPlus, optionIsMonadPlus, tryIsMonadPlus}
import monadics.instances._
import monadics.laws.MonadLaws
import monadics.structures.Monad
import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

import scala.language.higherKinds
import scala.util.Try

abstract class MonadLawsSpec[M[_]](name: String)
																	(implicit monad: Monad[M],
																	 arbMonadInt: Arbitrary[M[Int]],
																	 arbIntToMonadString: Arbitrary[Int => M[String]],
																	 arbStringToMonadLong: Arbitrary[String => M[Long]])
	extends PropSpec with PropertyChecks with Matchers {

	val laws = MonadLaws[M]

	property(s"$name - monad left identity") {
		forAll { (a: Int, f: Int => M[String]) =>
			laws.monadLeftIdentity(a, f)
		}
	}

	property(s"$name - monad right identity") {
		forAll { (ma: M[Int]) =>
			laws.monadRightIdentity(ma)
		}
	}

	property(s"$name - monad associativity") {
		forAll { (ma: M[Int], f: Int => M[String], g: String => M[Long]) =>
			laws.monadAssociativity(ma, f, g)
		}
	}
}

class ListMonadSpec extends MonadLawsSpec[List]("List")
class OptionMonadSpec extends MonadLawsSpec[Option]("Option")
class TryMonadSpec extends MonadLawsSpec[Try]("Try")
class FunctionMonadSpec extends MonadLawsSpec[Int => ?]("Int => ?")
class IdentityMonadSpec extends MonadLawsSpec[Identity]("Identity")
class OptionTMonadSpec extends MonadLawsSpec[OptionT[List, ?]]("OptionT[List, ?]")
class StateMonadSpec extends MonadLawsSpec[State[Int, ?]]("State[Int, ?]")
class StateTMonadSpec extends MonadLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
class TreeMonadSpec extends MonadLawsSpec[Tree]("Tree")
