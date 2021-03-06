package monadics.test.laws

import monadics.instances._
import monadics.instances.either._
import monadics.instances.list._
import monadics.instances.monoids.values._
import monadics.instances.option._
import monadics.instances.tryMonad._
import monadics.laws.FunctorLaws
import monadics.structures.{Equals, Functor}
import org.scalacheck.Arbitrary

import scala.language.higherKinds
import scala.util.Try

trait FunctorLawsSpec[F[_]] extends LawSpec {

	implicit val instance: Functor[F]
	implicit val arbIntInstance: Arbitrary[F[Int]]
	implicit val eqInt: Equals[F[Int]]
	implicit val eqLong: Equals[F[Long]]

	val laws: FunctorLaws[F] = FunctorLaws[F]

	property(s"$name - functor identity") {
		forAll { (xs: F[Int]) =>
			laws.functorIdentity(xs).isEqual shouldBe true
		}
	}

	property(s"$name - functor composition") {
		forAll { (xs: F[Int], f: Int => String, g: String => Long) =>
			laws.functorComposition(xs, f, g).isEqual shouldBe true
		}
	}
}

abstract class AbstractFunctorLawsSpec[F[_]](override val name: String)
																						(implicit override val instance: Functor[F],
																						 override val arbIntInstance: Arbitrary[F[Int]],
																						 override val eqInt: Equals[F[Int]],
																						 override val eqLong: Equals[F[Long]])
	extends FunctorLawsSpec[F]

class ListFunctorSpec extends AbstractFunctorLawsSpec[List]("List")
class OptionFunctorSpec extends AbstractFunctorLawsSpec[Option]("Option")
class TryFunctorSpec extends AbstractFunctorLawsSpec[Try]("Try")
class EitherFunctorSpec extends AbstractFunctorLawsSpec[Either[Int, ?]]("Either")
class EitherLeftFunctorSpec extends AbstractFunctorLawsSpec[LeftEither[?, Int]]("LeftEither")
class EitherRightFunctorSpec extends AbstractFunctorLawsSpec[RightEither[Int, ?]]("RightEither")
class IdentityFunctorSpec extends AbstractFunctorLawsSpec[Identity]("Identity")
class OptionTFunctorSpec extends AbstractFunctorLawsSpec[OptionT[List, ?]]("OptionT[List, ?]")
class StateFunctorSpec extends AbstractFunctorLawsSpec[State[Int, ?]]("State[Int, ?]")
class StateTFunctorSpec extends AbstractFunctorLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
class TreeFunctorSpec extends AbstractFunctorLawsSpec[Tree]("Tree")
class NonEmptyListFunctorSpec extends AbstractFunctorLawsSpec[NonEmptyList]("NonEmptyList")
class WriterFunctorSpec extends AbstractFunctorLawsSpec[Writer[String, ?]]("Writer[String, ?]")
class ReaderFunctorSpec extends AbstractFunctorLawsSpec[Reader[Int, ?]]("Reader[Int, ?]")(Reader.readerIsMonad, arbReader[Int], Reader.readerIsEquals, Reader.readerIsEquals)
class ContinuationFunctorSpec extends AbstractFunctorLawsSpec[Continuation[Int, ?]]("Continuation[Int, ?]")
