package monadics.test.laws

import monadics.ScalaMonoids.stringIsMonoid
import monadics.instances._
import monadics.instances.either._
import monadics.instances.list._
import monadics.instances.option._
import monadics.instances.tryMonad._
import monadics.laws.ApplicativeLaws
import monadics.structures.Applicative
import org.scalacheck.Arbitrary

import scala.language.higherKinds
import scala.util.Try

trait ApplicativeLawsSpec[App[_]] extends FunctorLawsSpec[App] {

	override val laws = ApplicativeLaws[App]
	implicit val instance: Applicative[App]
	implicit val arbIntToStringInstance: Arbitrary[App[Int => String]]

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

abstract class AbstractApplicativeLawsSpec[App[_]](override val name: String)
																					(implicit override val instance: Applicative[App],
																					 override val arbIntInstance: Arbitrary[App[Int]],
																					 override val arbIntToStringInstance: Arbitrary[App[Int => String]])
	extends ApplicativeLawsSpec[App]

class ListApplicativeSpec extends AbstractApplicativeLawsSpec[List]("List")
class OptionApplicativeSpec extends AbstractApplicativeLawsSpec[Option]("Option")
class TryApplicativeSpec extends AbstractApplicativeLawsSpec[Try]("Try")
class EitherApplicativeSpec extends AbstractApplicativeLawsSpec[Either[Int, ?]]("Either")
class IdentityApplicativeSpec extends AbstractApplicativeLawsSpec[Identity]("Identity")
class OptionTApplicativeSpec extends AbstractApplicativeLawsSpec[OptionT[List, ?]]("OptionT[List, ?]")
class StateApplicativeSpec extends AbstractApplicativeLawsSpec[State[Int, ?]]("State[Int, ?]")
class StateTApplicativeSpec extends AbstractApplicativeLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
class TreeApplicativeSpec extends AbstractApplicativeLawsSpec[Tree]("Tree")
class NonEmptyListApplicativeSpec extends AbstractApplicativeLawsSpec[NonEmptyList]("NonEmptyList")
class WriterApplicativeSpec extends AbstractApplicativeLawsSpec[Writer[String, ?]]("Writer[String, ?]")
class ReaderApplicativeSpec extends AbstractApplicativeLawsSpec[Reader[Int, ?]]("Reader[Int, ?]")
class ContinuationApplicativeSpec extends AbstractApplicativeLawsSpec[Continuation[Int, ?]]("Continuation[Int, ?]")
