package monadics.test.laws

import monadics.instances._
import monadics.instances.either._
import monadics.instances.list._
import monadics.instances.monoids.values.{stringIsMonoid, _}
import monadics.instances.option._
import monadics.instances.tryMonad._
import monadics.laws.ApplicativeLaws
import monadics.structures.{Applicative, Equals}
import org.scalacheck.Arbitrary

import scala.language.higherKinds
import scala.util.Try

trait ApplicativeLawsSpec[App[_]] extends FunctorLawsSpec[App] {

	override val laws: ApplicativeLaws[App] = ApplicativeLaws[App]
	implicit val instance: Applicative[App]
	implicit val arbIntToStringInstance: Arbitrary[App[Int => String]]
	implicit val arbStringToLongInstance: Arbitrary[App[String => Long]]
	implicit val eqString: Equals[App[String]]

	property(s"$name - applicative identity") {
		forAll { (xs: App[Int]) =>
			laws.applicativeIdentity(xs).isEqual shouldBe true
		}
	}

	property(s"$name - applicative homomorphism") {
		forAll { (a: Int, f: Int => String) =>
			laws.applicativeHomomorphism(a, f).isEqual shouldBe true
		}
	}

	property(s"$name - applicative interchange") {
		forAll { (a: Int, appF: App[Int => String]) =>
			laws.applicativeInterchange(a, appF).isEqual shouldBe true
		}
	}

	property(s"$name - applicative map") {
		forAll { (appA: App[Int], f: Int => String) =>
			laws.applicativeMap(appA, f).isEqual shouldBe true
		}
	}

	property(s"$name - applicative composition") {
		forAll(sizeRange(20)) { (appAToB: App[String => Long], appCToA: App[Int => String], appC: App[Int]) =>
			laws.applicativeComposition(appAToB, appCToA, appC)
		}
	}
}

abstract class AbstractApplicativeLawsSpec[App[_]](override val name: String)
																					(implicit override val instance: Applicative[App],
																					 override val arbIntInstance: Arbitrary[App[Int]],
																					 override val arbIntToStringInstance: Arbitrary[App[Int => String]],
																					 override val arbStringToLongInstance: Arbitrary[App[String => Long]],
																					 override val eqInt: Equals[App[Int]],
																					 override val eqLong: Equals[App[Long]],
																					 override val eqString: Equals[App[String]])
	extends ApplicativeLawsSpec[App]

class ListApplicativeSpec extends AbstractApplicativeLawsSpec[List]("List")
class OptionApplicativeSpec extends AbstractApplicativeLawsSpec[Option]("Option")
class TryApplicativeSpec extends AbstractApplicativeLawsSpec[Try]("Try")
class EitherApplicativeSpec extends AbstractApplicativeLawsSpec[Either[Int, ?]]("Either")
class EitherLeftApplicativeSpec extends AbstractApplicativeLawsSpec[LeftEither[?, Int]]("LeftEither")
class EitherRightApplicativeSpec extends AbstractApplicativeLawsSpec[RightEither[Int, ?]]("RightEither")
class IdentityApplicativeSpec extends AbstractApplicativeLawsSpec[Identity]("Identity")
class OptionTApplicativeSpec extends AbstractApplicativeLawsSpec[OptionT[List, ?]]("OptionT[List, ?]")
class StateApplicativeSpec extends AbstractApplicativeLawsSpec[State[Int, ?]]("State[Int, ?]")
class StateTApplicativeSpec extends AbstractApplicativeLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
class TreeApplicativeSpec extends AbstractApplicativeLawsSpec[Tree]("Tree")
class NonEmptyListApplicativeSpec extends AbstractApplicativeLawsSpec[NonEmptyList]("NonEmptyList")
class WriterApplicativeSpec extends AbstractApplicativeLawsSpec[Writer[String, ?]]("Writer[String, ?]")
class ReaderApplicativeSpec extends AbstractApplicativeLawsSpec[Reader[Int, ?]]("Reader[Int, ?]")
class ContinuationApplicativeSpec extends AbstractApplicativeLawsSpec[Continuation[Int, ?]]("Continuation[Int, ?]")
