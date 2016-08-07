import monadics.instances.OptionT
import monadics.ScalaMonads.tryIsMonadPlus
import monadics.instances.optionTMonad.optionTIsMonadPlus
import monadics.instances.optionTMonad.optionTIsMonadTrans

import scala.util.{Failure, Try}

def theVal: Int = 6
def theTry: Try[Int] = Try(5)
def theOption: Option[Int] = Option(4)
def theTryOption: Try[Option[Int]] = Try(Option(3))
def theFailure: Try[Int] = Failure(new Exception("foobar"))
def theEmptyTry: Try[Option[Int]] = Try(Option.empty)

val res: OptionT[Try, (Int, Int, Int, Int)] = for {
	x1 <- OptionT.create(theVal)
	x2 <- OptionT.lift(theTry)
	x3 <- OptionT(Try(theOption))
	x4 <- OptionT(theTryOption)
} yield (x1, x2, x3, x4)
val result: Try[Option[(Int, Int, Int, Int)]] = res.get

val resFailure = for {
	x1 <- OptionT.create(theVal)
	x2 <- OptionT.lift(theFailure)
	x3 <- OptionT(Try(theOption))
	x4 <- OptionT(theTryOption)
} yield (x1, x2, x3, x4)
val resultFailure = resFailure.get

val resEmptyTry = for {
	x1 <- OptionT.create(theVal)
	x2 <- OptionT.lift(theTry)
	x3 <- OptionT(Try(theOption))
	x4 <- OptionT(theEmptyTry)
} yield (x1, x2, x3, x4)
val resultEmptyTry = resEmptyTry.get
