package monadics.test.laws

import monadics.ScalaMonads.{listIsMonadPlus, optionIsMonadPlus, tryIsMonadPlus}
import monadics.instances._
import monadics.laws.AlternativeLaws
import monadics.structures.Alternative
import org.scalacheck.Arbitrary
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

import scala.language.higherKinds
import scala.util.Try

abstract class AlternativeLawsSpec[Alt[_]](name: String)
																					(implicit alternative: Alternative[Alt],
																					 arbAltInt: Arbitrary[Alt[Int]])
	extends PropSpec with PropertyChecks with Matchers {

	val laws = AlternativeLaws[Alt]

	property(s"$name - alternative left empty") {
		forAll { (alt: Alt[Int]) =>
			laws.alternativeLeftEmpty(alt)
		}
	}

	property(s"$name - alternative right empty") {
		forAll { (alt: Alt[Int]) =>
			laws.alternativeRightEmpty(alt)
		}
	}

	property(s"$name - alternative associativity") {
		forAll { (altA: Alt[Int], altB: Alt[Int], altC: Alt[Int]) =>
			laws.alternativeAssociativity(altA, altB, altC)
		}
	}
}

class ListAlternativeSpec extends AlternativeLawsSpec[List]("List")
class OptionAlternativeSpec extends AlternativeLawsSpec[Option]("Option")
class TryAlternativeSpec extends AlternativeLawsSpec[Try]("Try")
class OptionTAlternativeSpec extends AlternativeLawsSpec[OptionT[List, ?]]("OptionT[List, ?]")
class StateTAlternativeSpec extends AlternativeLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
