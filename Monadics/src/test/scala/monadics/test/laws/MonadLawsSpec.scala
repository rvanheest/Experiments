package monadics.test.laws

import monadics.instances._
import monadics.instances.either._
import monadics.instances.list._
import monadics.instances.option._
import monadics.instances.tryMonad._
import monadics.instances.monoids.values._
import monadics.laws.MonadLaws
import monadics.structures.{Equals, Monad}
import org.scalacheck.Arbitrary

import scala.language.higherKinds
import scala.util.Try

trait MonadLawsSpec[M[_]] extends ApplicativeLawsSpec[M] {

	override val laws: MonadLaws[M] = MonadLaws[M]
	implicit val instance: Monad[M]
	implicit val arbIntToMonadStringInstance: Arbitrary[Int => M[String]]
	implicit val arbStringToMonadLongInstance: Arbitrary[String => M[Long]]

	property(s"$name - monad left identity") {
		forAll { (a: Int, f: Int => M[String]) =>
			laws.monadLeftIdentity(a, f).isEqual shouldBe true
		}
	}

	property(s"$name - monad right identity") {
		forAll { (ma: M[Int]) =>
			laws.monadRightIdentity(ma).isEqual shouldBe true
		}
	}

	property(s"$name - monad associativity") {
		forAll { (ma: M[Int], f: Int => M[String], g: String => M[Long]) =>
			laws.monadAssociativity(ma, f, g).isEqual shouldBe true
		}
	}
}

abstract class AbstractMonadLawsSpec[M[_]](override val name: String)
																					(implicit override val instance: Monad[M],
																					 override val arbIntInstance: Arbitrary[M[Int]],
																					 override val arbIntToStringInstance: Arbitrary[M[Int => String]],
																					 override val arbIntToMonadStringInstance: Arbitrary[Int => M[String]],
																					 override val arbStringToMonadLongInstance: Arbitrary[String => M[Long]],
																					 override val arbStringToLongInstance: Arbitrary[M[String => Long]],
																					 override val eqInt: Equals[M[Int]],
																					 override val eqLong: Equals[M[Long]],
																					 override val eqString: Equals[M[String]])
	extends MonadLawsSpec[M]

class ListMonadSpec extends AbstractMonadLawsSpec[List]("List")
class OptionMonadSpec extends AbstractMonadLawsSpec[Option]("Option")
class TryMonadSpec extends AbstractMonadLawsSpec[Try]("Try")
class EitherMonadSpec extends AbstractMonadLawsSpec[Either[Int, ?]]("Either")
class EitherLeftMonadSpec extends AbstractMonadLawsSpec[LeftEither[?, Int]]("LeftEither")
class EitherRightMonadSpec extends AbstractMonadLawsSpec[RightEither[Int, ?]]("RightEither")
class IdentityMonadSpec extends AbstractMonadLawsSpec[Identity]("Identity")
class OptionTMonadSpec extends AbstractMonadLawsSpec[OptionT[List, ?]]("OptionT[List, ?]")
class StateMonadSpec extends AbstractMonadLawsSpec[State[Int, ?]]("State[Int, ?]")
class StateTMonadSpec extends AbstractMonadLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
class TreeMonadSpec extends AbstractMonadLawsSpec[Tree]("Tree")
class NonEmptyListMonadSpec extends AbstractMonadLawsSpec[NonEmptyList]("NonEmptyList")
class WriterMonadSpec extends AbstractMonadLawsSpec[Writer[String, ?]]("Writer[String, ?]")
class ReaderMonadSpec extends AbstractMonadLawsSpec[Reader[Int, ?]]("Reader[Int, ?]")
class ContinuationMonadSpec extends AbstractMonadLawsSpec[Continuation[Int, ?]]("Continuation[Int, ?]")
