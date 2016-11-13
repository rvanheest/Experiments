package monadics.test.laws

import monadics.ScalaMonads.{functionIsMonad, listIsMonadPlus, optionIsMonadPlus, tryIsMonadPlus}
import monadics.ScalaMonoids.stringIsMonoid
import monadics.instances._
import monadics.laws.{MonadFailLaws, MonadLaws}
import monadics.structures.{Monad, MonadFail}
import org.scalacheck.Arbitrary

import scala.language.higherKinds
import scala.util.Try

trait MonadFailLawsSpec[M[_]] extends MonadLawsSpec[M] {

  override val laws = MonadFailLaws[M]
  implicit val instance: MonadFail[M]
  implicit val arbIntToMonadStringInstance: Arbitrary[Int => M[String]]

  property(s"$name - monadfail left zero") {
    forAll { (s: String, f: Int => M[String]) =>
      laws.monadFailLeftZero(s, f)
    }
  }
}

abstract class AbstractMonadFailLawsSpec[M[_]](override val name: String)
                                              (implicit override val instance: MonadFail[M],
                                               override val arbIntInstance: Arbitrary[M[Int]],
                                               override val arbIntToStringInstance: Arbitrary[M[Int => String]],
                                               override val arbIntToMonadStringInstance: Arbitrary[Int => M[String]],
                                               override val arbStringToMonadLongInstance: Arbitrary[String => M[Long]])
  extends MonadFailLawsSpec[M]

class ListMonadFailSpec extends AbstractMonadFailLawsSpec[List]("List")
class OptionMonadFailSpec extends AbstractMonadFailLawsSpec[Option]("Option")
class TryMonadFailSpec extends AbstractMonadFailLawsSpec[Try]("Try")
class StateTMonadFailSpec extends AbstractMonadFailLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
