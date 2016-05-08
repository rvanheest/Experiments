import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration.Duration
import scala.util.Try

// we do three things with the outer container:
// * map
// * flatMap
// * create a new one

trait Monad[M[_]] {
  def map[A, B](ma: M[A])(f: A => B): M[B]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def create[A](a: A): M[A]
}

// we can now create a wrapper class that nests an Option in any Monad
case class OptionT[M[_], A](run: M[Option[A]])(implicit m: Monad[M]) {
  import OptionT.lift

  def map[B](f: A => B): OptionT[M, B] = {
    lift(m.map(run)(_.map(f)))
  }

  def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] = {
    lift(m.flatMap(run)(_.map(f.andThen(_.run)).getOrElse(m.create(None))))
  }
}
object OptionT {
  def lift[M[_], A](x: M[Option[A]])(implicit m: Monad[M]): OptionT[M, A] = new OptionT[M, A](x)
}

// some instances of monad
implicit def futureIsMonad(implicit executor: ExecutionContext) = new Monad[Future] {
  def map[A, B](ma: Future[A])(f: (A) => B): Future[B] = ma map f

  def flatMap[A, B](ma: Future[A])(f: (A) => Future[B]): Future[B] = ma flatMap f

  def create[A](a: A): Future[A] = Future.successful(a)
}

implicit def tryIsMonad = new Monad[Try] {
  def map[A, B](ma: Try[A])(f: (A) => B): Try[B] = ma map f

  def flatMap[A, B](ma: Try[A])(f: (A) => Try[B]): Try[B] = ma flatMap f

  def create[A](a: A): Try[A] = Try(a)
}

val to1 = Try(Option(5))
val to2 = Try(Option(3))
val toRes = for {
  x <- OptionT.lift(to1)
  y <- OptionT.lift(to2)
} yield x + y
toRes.run

//////////////////////////////////////////////////

// you can't combine these with for-comprehensions as they have different shapes
def getA: Future[Option[Int]] = Future.successful(Option(5))
def getB: Option[Int] = Option(3)
def getC: Future[Int] = Future.successful(1)

// look for the 'richest' type and put the others in that

val result: OptionT[Future, Int] = for {
  a <- OptionT.lift(getA)
  b <- OptionT.lift(Future.successful(getB))
  c <- OptionT.lift(getC.map(i => Option(i)))
} yield a + b + c
