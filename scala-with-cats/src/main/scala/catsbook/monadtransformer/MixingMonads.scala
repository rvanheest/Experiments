package catsbook.monadtransformer

import cats.data.{ EitherT, OptionT }
import cats.instances.either._
import cats.instances.future._
import cats.instances.list._
import cats.syntax.applicative._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import scala.language.postfixOps

object MixingMonads extends App {

  case class Error(msg: String)
  case class User(name: String)

  def lookupUser(id: Long): Either[Error, Option[User]] = Right(Some(User("my-name")))

  def lookupUserName(id: Long): Either[Error, Option[String]] = {
    lookupUser(id).map(_.map(_.name))
  }

  println(lookupUserName(123)) // Right(Some("my-name"))

  type ListOption[A] = OptionT[List, A]

  val result1: ListOption[Int] = OptionT(List(Option(10)))
  val result2: ListOption[Int] = 32.pure[ListOption]

  println {
    for {
      x <- result1
      y <- result2
    } yield x + y
  } // OptionT(List(Some(42)))

  type ErrorOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A] // Either[String, Option[A]]

  println {
    for {
      x <- 10.pure[ErrorOrOption]
      y <- 32.pure[ErrorOrOption]
    } yield x + y
  } // OptionT(Right(Some(42)))

  type FutureEither[A] = EitherT[Future, String, A] // Future[Either[String, A]]
  type FutureEitherOption[A] = OptionT[FutureEither, A] // Future[Either[String, Option[A]]]

  val futureEitherOption = for {
    x <- 10.pure[FutureEitherOption]
    y <- 32.pure[FutureEitherOption]
  } yield x + y

  println(Await.result(futureEitherOption.value.value, 10 seconds)) // Right(Some(42))

  val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
  val errorStack2 = 32.pure[ErrorOrOption]

  println(errorStack1.value) // Right(Some(10))
  println(errorStack2.value.map(_.getOrElse(-1))) // Right(32)
}
