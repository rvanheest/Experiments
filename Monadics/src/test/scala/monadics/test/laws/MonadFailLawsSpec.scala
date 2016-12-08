package monadics.test.laws

import monadics.instances._
import monadics.instances.list._
import monadics.instances.option._
import monadics.instances.tryMonad._
import monadics.instances.monoids.values._
import monadics.laws.MonadFailLaws
import monadics.structures.{Equals, MonadFail}
import org.scalacheck.Arbitrary

import scala.language.higherKinds
import scala.util.Try

trait MonadFailLawsSpec[MF[_]] extends MonadLawsSpec[MF] {

  override val laws: MonadFailLaws[MF] = MonadFailLaws[MF]
  implicit val instance: MonadFail[MF]
  implicit val arbIntToMonadStringInstance: Arbitrary[Int => MF[String]]

  property(s"$name - monadfail left zero") {
    forAll { (s: String, f: Int => MF[String]) =>
      laws.monadFailLeftZero(s, f).isEqual shouldBe true
    }
  }
}

abstract class AbstractMonadFailLawsSpec[MF[_]](override val name: String)
                                              (implicit override val instance: MonadFail[MF],
                                               override val arbIntInstance: Arbitrary[MF[Int]],
                                               override val arbIntToStringInstance: Arbitrary[MF[Int => String]],
                                               override val arbIntToMonadStringInstance: Arbitrary[Int => MF[String]],
                                               override val arbStringToMonadLongInstance: Arbitrary[String => MF[Long]],
                                               override val arbStringToLongInstance: Arbitrary[MF[String => Long]],
                                               override val eqInt: Equals[MF[Int]],
                                               override val eqLong: Equals[MF[Long]],
                                               override val eqString: Equals[MF[String]])
  extends MonadFailLawsSpec[MF]

class ListMonadFailSpec extends AbstractMonadFailLawsSpec[List]("List")
class OptionMonadFailSpec extends AbstractMonadFailLawsSpec[Option]("Option")
class TryMonadFailSpec extends AbstractMonadFailLawsSpec[Try]("Try")
class StateTMonadFailSpec extends AbstractMonadFailLawsSpec[StateT[Int, ?, List]]("StateT[Int, ?, List]")
