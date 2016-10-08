package monadics.test.laws

import monadics.ScalaMonads.{functionIsMonad, listIsMonadPlus, optionIsMonadPlus, tryIsMonadPlus}
import monadics.instances._
import monadics.laws.MonadLaws
import monadics.structures.Monad
import org.scalacheck.Arbitrary

import scala.language.higherKinds
import scala.util.Try

trait MonadLawsSpec[M[_]] extends ApplicativeLawsSpec[M] {

	override val laws = MonadLaws[M]
	implicit val instance: Monad[M]
	implicit val arbIntToMonadStringInstance: Arbitrary[Int => M[String]]
	implicit val arbStringToMonadLongInstance: Arbitrary[String => M[Long]]

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

abstract class AbstractMonadLawsSpec[M[_]](override val name: String)
																	(implicit override val instance: Monad[M],
																	 override val arbIntInstance: Arbitrary[M[Int]],
																	 override val arbIntToStringInstance: Arbitrary[M[Int => String]],
																	 override val arbIntToMonadStringInstance: Arbitrary[Int => M[String]],
																	 override val arbStringToMonadLongInstance: Arbitrary[String => M[Long]])
	extends MonadLawsSpec[M]

class ListMonadSpec extends AbstractMonadLawsSpec[List]("List")
class OptionMonadSpec extends AbstractMonadLawsSpec[Option]("Option")
class TryMonadSpec extends AbstractMonadLawsSpec[Try]("Try")
class FunctionMonadSpec extends AbstractMonadLawsSpec[Int => ?]("Int => ?")
class IdentityMonadSpec extends AbstractMonadLawsSpec[Identity]("Identity")
class OptionTMonadSpec extends AbstractMonadLawsSpec[OptionT[List, ?]]("OptionT[List, ?]")
class StateMonadSpec extends AbstractMonadLawsSpec[State[Int, ?]]("State[Int, ?]")
class StateTMonadSpec extends AbstractMonadLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
class TreeMonadSpec extends AbstractMonadLawsSpec[Tree]("Tree")
class NonEmptyListMonadSpec extends AbstractMonadLawsSpec[NonEmptyList]("NonEmptyList")
