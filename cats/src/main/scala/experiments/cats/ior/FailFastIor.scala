package experiments.cats.ior

import cats.data.{ Ior, IorNec, NonEmptyChain }
import cats.syntax.either._
import cats.syntax.ior._

import scala.annotation.tailrec
import scala.util.{ Failure, Success, Try }

object FailFastIor extends App {

  val xs: List[Try[Int]] = List(
    Success(1),
    Success(2),
    Failure(new Exception("a")),
    Success(3),
    Failure(new Exception("b"))
  )

  val ys: List[Try[Int]] = List(
    Success(1),
    Success(2),
    Success(3)
  )

  val zs: List[Try[Int]] = List(
    Failure(new Exception("a")),
    Failure(new Exception("b"))
  )

  println("collectResults")
  println(xs.collectResults)
  println(ys.collectResults)
  println(zs.collectResults)
  println
  println("failFast")
  println(xs.failFast)
  println(ys.failFast)
  println(zs.failFast)

  def collectResults[T](xs: Seq[Try[T]]): IorNec[Throwable, Seq[T]] = {

    @tailrec
    def tailrec(xs: Seq[Try[T]], result: IorNec[Throwable, Seq[T]]): IorNec[Throwable, Seq[T]] = {
      (xs, result) match {
        case (Seq(), _) => result
        case (Seq(Success(h), tail @ _*), Ior.Left(es)) => tailrec(tail, Ior.both(es, Seq(h)))
        case (Seq(Success(h), tail @ _*), Ior.Right(ts)) => tailrec(tail, Ior.right(ts :+ h))
        case (Seq(Success(h), tail @ _*), Ior.Both(es, ts)) => tailrec(tail, Ior.both(es, ts :+ h))
        case (Seq(Failure(e), tail @ _*), Ior.Left(es)) => tailrec(tail, Ior.left(es :+ e))
        case (Seq(Failure(e), tail @ _*), Ior.Right(ts)) => tailrec(tail, Ior.both(NonEmptyChain.one(e), ts))
        case (Seq(Failure(e), tail @ _*), Ior.Both(es, ts)) => tailrec(tail, Ior.both(es :+ e, ts))
      }
    }

    xs match {
      case Seq() => Ior.fromEither(Seq.empty[T].rightNec[Throwable])
      case Seq(Success(h), tail @ _*) => tailrec(tail, Ior.fromEither(Seq(h).rightNec[Throwable]))
      case Seq(Failure(e), tail @ _*) => tailrec(tail, Ior.fromEither(e.leftNec[Seq[T]]))
    }
  }

  def failFast[T](xs: Seq[Try[T]]): Ior[Throwable, Seq[T]] = {
    @tailrec
    def tailrec(xs: Seq[Try[T]], soFar: Seq[T] = Seq.empty): Ior[Throwable, Seq[T]] = {
      xs match {
        case Seq() => soFar.rightIor
        case Seq(Success(h), tail @ _*) => tailrec(tail, soFar :+ h)
        case Seq(Failure(e), _ @ _*) if soFar.isEmpty => e.leftIor
        case Seq(Failure(e), _ @ _*) => Ior.both(e, soFar)
      }
    }

    tailrec(xs)
  }

  implicit class FailSyntax[T](val xs: Seq[Try[T]]) extends AnyVal {
    def collectResults: IorNec[Throwable, Seq[T]] = FailFastIor.collectResults(xs)

    def failFast: Ior[Throwable, Seq[T]] = FailFastIor.failFast(xs)
  }
}
