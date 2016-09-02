package monadics.test.laws

import monadics.ScalaMonads.{functionIsMonadPlus, listIsMonadPlus, optionIsMonadPlus, tryIsMonadPlus}
import monadics.instances._
import monadics.laws.FunctorLaws
import monadics.structures.Functor
import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

import scala.language.higherKinds
import scala.util.Try

abstract class FunctorLawsSpec[F[_]](name: String)
																		(implicit functor: Functor[F], arbFInt: Arbitrary[F[Int]])
	extends PropSpec with PropertyChecks with Matchers {

	val laws = FunctorLaws[F]

	property(s"$name - identity") {
		forAll { (xs: F[Int]) =>
			laws.identity(xs)
		}
	}

	property(s"$name - composition") {
		forAll { (xs: F[Int], f: Int => String, g: String => Long) =>
			laws.composition(xs, f, g)
		}
	}
}

class ListFunctorSpec extends FunctorLawsSpec[List]("List")
class OptionFunctorSpec extends FunctorLawsSpec[Option]("Option")
class TryFunctorSpec extends FunctorLawsSpec[Try]("Try")
class FunctionFunctorSpec extends FunctorLawsSpec[Int => ?]("Int => ?")
class IdentityFunctorSpec extends FunctorLawsSpec[Identity]("Identity")
class OptionTFunctorSpec extends FunctorLawsSpec[OptionT[List, ?]]("OptionT[List, ?]")
class StateFunctorSpec extends FunctorLawsSpec[State[Int, ?]]("State[Int, ?]")
class StateTFunctorSpec extends FunctorLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
class TreeFunctorSpec extends FunctorLawsSpec[Tree]("Tree")
