import monadics.instances.OptionT
import monadics.instances.OptionT.optionTIsMonadTrans
import monadics.instances.either._
import monadics.instances.tryMonad._

import scala.util.{Failure, Try}

def theVal: Int = 6
def theTry: Try[Int] = Try(5)
def theOption: Option[Int] = Option(4)
def theTryOption: Try[Option[Int]] = Try(Option(3))
def theFailure: Try[Int] = Failure(new Exception("foobar"))
def theEmptyTry: Try[Option[Int]] = Try(Option.empty)
def theLeftEither: Either[String, Int] = Left("some kind of failure")
def theRightEither: Either[String, Int] = Right(2)


// TODO I'm not sure why we need this type alias here!
type Foo[+T] = Either[String, T]

val resEitherSuccess = {
	for {
		x1 <- OptionT.create[Foo, Int](theVal)
		x2 <- OptionT.lift[Foo, Int](theRightEither)
		x3 <- OptionT[Foo, Int](Right(theOption))
	} yield (x1, x2, x3)
}
val resultEitherSuccess: Either[String, Option[(Int, Int, Int)]] = resEitherSuccess.get

val resEitherFail = {
	for {
		x1 <- OptionT.create[Foo, Int](theVal)
		x2 <- OptionT.lift[Foo, Int](theLeftEither)
		x3 <- OptionT[Foo, Int](Right(theOption))
	} yield (x1, x2, x3)
}
val resultEitherFail: Either[String, Option[(Int, Int, Int)]] = resEitherFail.get

val res: OptionT[Try, (Int, Int, Int, Int)] = {
	for {
		x1 <- OptionT.create(theVal)
		x2 <- OptionT.lift(theTry)
		x3 <- OptionT(Try(theOption))
		x4 <- OptionT(theTryOption)
	} yield (x1, x2, x3, x4)
}
val result: Try[Option[(Int, Int, Int, Int)]] = res.get

val resFailure = {
	for {
		x1 <- OptionT.create(theVal)
		x2 <- OptionT.lift(theFailure)
		x3 <- OptionT(Try(theOption))
		x4 <- OptionT(theTryOption)
	} yield (x1, x2, x3, x4)
}
val resultFailure = resFailure.get

val resEmptyTry = {
	for {
		x1 <- OptionT.create(theVal)
		x2 <- OptionT.lift(theTry)
		x3 <- OptionT(Try(theOption))
		x4 <- OptionT(theEmptyTry)
	} yield (x1, x2, x3, x4)
}
val resultEmptyTry = resEmptyTry.get


