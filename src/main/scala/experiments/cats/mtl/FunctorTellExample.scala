package experiments.cats.mtl

import cats._
import cats.data._
import cats.implicits._
import cats.mtl.FunctorTell
import cats.mtl.implicits._

import scala.language.higherKinds

object FunctorTellExample extends App {
  case class ServiceParams(option1: String, option2: Int)

  case class ServiceResult(userId: Int, companies: List[String])

  // a fake call to some external service, replace with real implementation
  def serviceCall[F[_] : Monad](params: ServiceParams): F[ServiceResult] = {
    ServiceResult(0, List("My Company")).pure[F]
  }

  type Logger[F[_]] = FunctorTell[F, Chain[String]]
  def Logger[F[_]](implicit F : Logger[F]): Logger[F] = F

  def serviceCallWithLog[F[_] : Logger : Monad](params: ServiceParams): F[ServiceResult] = {
    for {
      _ <- Logger[F].tell(Chain.one(show"Call to service with ${ params.option1 } and ${ params.option2 }"))
      result <- serviceCall[F](params)
      _ <- Logger[F].tell(Chain.one(show"Service returned: userId: ${ result.userId }; companies: ${ result.companies }"))
    } yield result
  }

  type Effect[A] = Writer[Chain[String], A]

  val (log, result): (Chain[String], ServiceResult) = serviceCallWithLog[Effect](ServiceParams("business", 42)).run

  println(log)
  println(result)
}
